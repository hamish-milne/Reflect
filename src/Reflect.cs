using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Linq.Expressions;
using System.Reflection;

namespace Reflect
{

#if NET35 || NET40
	// ReSharper disable once RedundantNameQualifier
	using TypeInfo = System.Type;
#endif

	internal static class TypeInfoEx
	{
#if NETSTANDARD1_0
		public static FieldInfo GetField(this TypeInfo type, string name)
			=> type.GetDeclaredField(name);

		public static ConstructorInfo GetConstructor(this TypeInfo type, Type[] arguments)
			=> type.DeclaredConstructors
			.FirstOrDefault(c => c.GetParameters().Select(p => p.ParameterType)
				.SequenceEqual(arguments));

		public static MethodInfo GetMethod(this TypeInfo type, string name)
			=> type.GetDeclaredMethod(name);

		public static IEnumerable<FieldInfo> GetFields(this Type type)
			=> type.GetRuntimeFields();

		public static bool IsInstanceOfType(this TypeInfo type, object obj)
			=> type.IsAssignableFrom(obj.GetType().GetTypeInfo());

		public static ConstructorInfo[] GetDeclaredConstructors(this TypeInfo type)
			=> type.DeclaredConstructors.ToArray();

		public static object CreateInstance(this TypeInfo type)
			=> type.GetConstructor(new Type[0]).Invoke(null);

		public static FieldInfo GetPrivateField(this Type type, string name)
			=> type.GetTypeInfo().GetField(name);

		public static MethodInfo GetMethod(this TypeInfo type, string name, Type[] parameters)
			=> type.AsType().GetRuntimeMethod(name, parameters);
#else

#if NET35 || NET40
		public static Type GetTypeInfo(this Type type)
			=> type;

		public static Type AsType(this Type type)
			=> type;

		public static ConstructorInfo GetConstructor(this Type type, Type[] args) =>
			type.GetConstructor(BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance, null, args, null);

		public static bool IsDefined(this MemberInfo member, Type attributeType, bool inherit = false)
			=> Attribute.IsDefined(member, attributeType, inherit);

		public static T GetCustomAttribute<T>(this MemberInfo member) where T : Attribute
			=> (T)Attribute.GetCustomAttribute(member, typeof(T));

		public static Delegate CreateDelegate(this MethodInfo method, Type delegateType, object target)
			=> Delegate.CreateDelegate(delegateType, target, method, true);

		public static Delegate CreateDelegate(this MethodInfo method, Type delegateType)
			=> Delegate.CreateDelegate(delegateType, method, true);

		public static IEnumerable<FieldInfo> GetRuntimeFields(this Type type)
			=> type.GetFields(
						BindingFlags.Instance |
						BindingFlags.Static |
						BindingFlags.Public |
						BindingFlags.NonPublic);

		public static MethodInfo GetDeclaredMethod(this Type type, string name)
			=> type.GetMethod(name,
				BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Static |
				BindingFlags.DeclaredOnly);

		public static IEnumerable<MethodInfo> GetRuntimeMethods(this Type type)
			=> type.GetMethods(BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Static);

#endif

		public static ConstructorInfo[] GetDeclaredConstructors(this Type type)
			=> type.GetConstructors(BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance);

		public static object CreateInstance(this Type type)
			=> Activator.CreateInstance(type, true);

		public static FieldInfo GetPrivateField(this Type type, string name)
			=> type.GetField(name, BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Static);

#endif
	}

	/// <summary>
	/// Apply to a method stub to look for a constructor rather than a method
	/// </summary>
	[AttributeUsage(AttributeTargets.Field)]
	public class ConstructorAttribute : Attribute { }

	// Currently it is not possible to create a dynamic method with a ref return, so this feature doesn't work
	// It can be re-enabled if and when this oversight is fixed
	/*
	/// <summary>
	/// Use this delegate to reflect a field of compatible type `T`
	/// </summary>
	/// <typeparam name="T"></typeparam>
	/// <returns></returns>
	public delegate ref T FieldStub<T>();
	*/

	/// <summary>
	/// Apply to a concrete suclass of <c>Reflect</c> to set the type it will reflect over
	/// </summary>
	[AttributeUsage(AttributeTargets.Class)]
	public class ReflectedTypeAttribute : Attribute
	{
		public string TypeName { get; set; }
		public string AssemblyName { get; set; }
		public Type Type { get; set; }
		public Type AssemblyType { get; set; }

		public ReflectedTypeAttribute(Type type) => Type = type;

		public ReflectedTypeAttribute(string typeName, Type assemblyType = null)
		{
			TypeName = typeName;
			AssemblyType = assemblyType;
		}
	}

	/// <summary>
	/// Base class for reflection stubs. Inherit and apply the <c>ReflectedTypeAttribute</c>.
	/// </summary>
	public abstract class ReflectStub
	{
		// Caches method references and instance delegate creators to generally speed up
		// the creation of new objects
		private class TypeCache
		{
			public readonly ConstructorInfo[] Ctors;
			public readonly MethodInfo[] InstanceMethods, StaticMethods;

			public readonly Dictionary<FieldInfo, Func<object, Delegate>> Creators = new Dictionary<FieldInfo, Func<object, Delegate>>();

			public TypeCache(TypeInfo type)
			{
				Ctors = type.GetDeclaredConstructors();
				InstanceMethods = type.AsType().GetRuntimeMethods().Where(m => !m.IsStatic).ToArray();
				StaticMethods = type.AsType().GetRuntimeMethods().Where(m => m.IsStatic).ToArray();
			}
		}

		// Map of reflected types to type caches
		private static readonly Dictionary<Type, TypeCache> Cache = new Dictionary<Type, TypeCache>();

		// Set of types that have been initialized, so we can check later
		private static readonly HashSet<Type> InitializedTypes = new HashSet<Type>();

		// A map of stub types to reflected types for quick access
		private static readonly Dictionary<Type, TypeInfo> ReflectedTypes = new Dictionary<Type, TypeInfo>();

		// Cache of stub type constructors used for conversion operations
		private static readonly Dictionary<Type, ConstructorInfo> StubConstructors = new Dictionary<Type, ConstructorInfo>();

		// Gets a type cache for the reflected type
		private static TypeCache GetTypeCache(Type stubType)
		{
			if(!Cache.TryGetValue(stubType, out var tc))
				Cache.Add(stubType, tc = new TypeCache(GetReflectedType(stubType)));
			return tc;
		}

		private static ConstructorInfo GetStubCtor(Type stubType)
		{
			var ti = stubType.GetTypeInfo();
			if(ti.IsAbstract) throw new Exception($"A stub return type was requested, but {stubType} is abstract");
			if(!StubConstructors.TryGetValue(stubType, out var ctor))
				StubConstructors.Add(stubType, ctor = ti.GetConstructor(new[] { typeof(object) }));
			if(ctor == null) throw new Exception($"The stub type {stubType} does not implement a constructor that accepts an object argument");
			return ctor;
		}

		// Map of reflected types to method stub fields
		private static readonly Dictionary<Type, FieldInfo[]> ValidFieldsCache = new Dictionary<Type, FieldInfo[]>();

		// The reflected instance
		private readonly object _instance;

		// The `_instance` field, for reflection interop
		private static readonly FieldInfo InstanceField =
			typeof(ReflectStub).GetPrivateField(nameof(_instance));

		/// <summary>
		/// Gets the reflected instance for the given stub instance
		/// </summary>
		/// <param name="r">The stub instance, or <c>null</c></param>
		/// <returns>The reflected instance, or <c>null</c></returns>
		public static object GetInstance(ReflectStub r) => r?._instance;

		/// <summary>
		/// Gets the reflected type for the given stub instance
		/// </summary>
		/// <param name="r">The stub instance, or <c>null</c></param>
		/// <returns>The reflected type, or <c>null</c></returns>
		public static TypeInfo GetReflectedType(ReflectStub r) => ReflectedTypes[r.GetType()];

		/// <summary>
		/// Gets the reflected type for the given stub type
		/// </summary>
		/// <param name="stubType">The stub type</param>
		/// <param name="exception">If <c>true</c>, throw an exception if a reflected type could not be found</param>
		/// <returns>The reflected type</returns>
		public static TypeInfo GetReflectedType(Type stubType, bool exception = true)
		{
			if (!ReflectedTypes.TryGetValue(stubType, out var rType))
			{
				var attr = stubType.GetTypeInfo().GetCustomAttribute<ReflectedTypeAttribute>();
				if (attr == null)
				{
					if (exception) throw new Exception($"Stub type {stubType} has no ReflectedTypeAttribute");
				}
				else
				{
					var type = attr.Type ?? GetType(attr.TypeName, attr.AssemblyType?.GetTypeInfo().Assembly);
					if (type == null && exception)
						throw new Exception($"Attribute for {stubType} does not define reflected type");
					rType = type.GetTypeInfo();
				}
				ReflectedTypes.Add(stubType, rType);
			}
			return rType;
		}
		
		// Gets a type by name
		private static Type GetType(string type, Assembly assembly)
		{
			if (assembly != null)
				return assembly.GetType(type);
#if NETSTANDARD1_0
			throw new Exception(".NET Core requires specifying an assembly type to load by name");
#else
			return AppDomain.CurrentDomain.GetAssemblies().Select(a =>
			{
				try
				{
					return a.GetType(type, false);
				}
				catch
				{
					return null;
				}
			}).FirstOrDefault(t => t != null) ?? throw new Exception($"Unable to find type {type}");
#endif
		}

		/// <summary>
		/// Creates a new instance, optionally passing in an existing reflected instance
		/// </summary>
		/// <param name="instance">An existing instance, or <c>null</c> to create a new one</param>
		protected ReflectStub(object instance = null)
		{
			var rType = GetReflectedType(GetType());
			InitializeStatic(rType.AsType());
			_instance = instance ?? (rType.IsAbstract ? null : rType.CreateInstance());
			if(_instance == null)
				throw new Exception($"Cannot create a reflected instance of an abstract type {rType}");
			if(!rType.IsInstanceOfType(_instance))
				throw new Exception($"The given object {_instance} is not an instance of reflected type {rType}");
			InitializeInstance();
		}

		/// <summary>
		/// Reflect stubs will compare equal where their instances (or types, where they're abstract) are equal
		/// </summary>
		/// <param name="obj"></param>
		/// <returns></returns>
		public override bool Equals(object obj)
		{
			if (ReferenceEquals(this, obj)) return true;
			var other = obj as ReflectStub;
			return other != null && _instance.Equals(other._instance);
		}

		/// <summary>
		/// Gets the reflected instance or type hashcode
		/// </summary>
		/// <returns></returns>
		public override int GetHashCode()
		{
			return (_instance ?? GetReflectedType(GetType())).GetHashCode();
		}

		private static FieldInfo[] GetAllFields(Type type)
		{
			if (!ValidFieldsCache.TryGetValue(type, out var fields))
			{
				fields = type.GetRuntimeFields()
					.Where(f => f.DeclaringType != null && f.DeclaringType.GetTypeInfo().IsSubclassOf(typeof(ReflectStub)))
					.Where(f => f.FieldType.GetTypeInfo().IsSubclassOf(typeof(Delegate)))
					.ToArray();
				ValidFieldsCache.Add(type, fields);
			}
			return fields;
		}

		// Sets the values of all stub fields
		private void InitializeInstance()
		{
			foreach (var f in GetAllFields(GetType()))
			{
				if (f.IsStatic || f.GetValue(this) != null) continue;
				if(f.IsDefined(typeof(ConstructorAttribute)))
					throw new Exception("Constructor stubs must be static");
				if(_instance == null) throw new Exception($"No instance provided for non-static field {f}");
				f.SetValue(this, GetDelegate(f, GetType(), _instance));
			}
		}

		/// <summary>
		/// Initializes static fields for the given type
		/// </summary>
		/// <param name="type"></param>
		protected static void InitializeStatic(Type type)
		{
			if (!InitializedTypes.Add(type)) return;
			foreach (var f in GetAllFields(type))
			{
				if (!f.IsStatic || f.GetValue(null) != null) continue;
				var del = f.IsDefined(typeof(ConstructorAttribute)) ? GetCtor(f, type) : GetDelegate(f, type, null);
				f.SetValue(null, del);
			}
		}

		protected static void InitializeStatic()
		{
			var frame = new StackFrame(1);
			var type = frame.GetMethod().DeclaringType;
			InitializeStatic(type);
		}

		// Checks if a candidate parameter matches the desired stub one
		private static bool ValidParameter(ParameterInfo desired, ParameterInfo candidate/*, ref bool requiresRefCast*/)
		{
			if (desired.ParameterType == candidate.ParameterType) return true;
			var desiredType = desired.ParameterType.GetTypeInfo();
			var candidateType = candidate.ParameterType.GetTypeInfo();

#if NET35
			if(desiredType.IsByRef)
				throw new Exception("ByRef arguments are not supported in .NET 3.5 unless the signature is exact");
#endif

			// Check for direct assignment capability
			var isAssignable = desired.Position < 0
				? desiredType.IsAssignableFrom(candidateType)
				: candidateType.IsAssignableFrom(desiredType);
			if (isAssignable)
				return true;

			#if !NET35
			// For the case of 'ref' or 'out' parameters
			if (candidate.ParameterType.IsByRef)
			{
				if (!desired.ParameterType.IsByRef) return false;
				if (desired.IsOut != candidate.IsOut) return false;

				var tDesiredElement = desiredType.GetElementType();
				var tCandidateElement = candidateType.GetElementType();

				if (tDesiredElement == tCandidateElement) return true;
				if (tDesiredElement.GetTypeInfo().IsAssignableFrom(tCandidateElement.GetTypeInfo()))
				{
					//requiresRefCast |= !desired.IsOut;
					return true;
				}

				if (desiredType.IsSubclassOf(typeof(ReflectStub)))
				{
					var tDesired = GetReflectedType(desired.ParameterType.GetElementType(), false);
					var tCandidate = candidate.ParameterType.GetElementType().GetTypeInfo();
					if (tDesired == null || tCandidate == null) return false;
					// For 'out' parameters, the invokee can be a subclass
					if (desired.IsOut)
						return tDesired.IsAssignableFrom(tCandidate);
					// for 'ref' parameters, they need to be identical
					return tDesired.AsType() == tCandidate.AsType();
				}
			}
			#endif

			// It might also be valid if we're passing in a Reflect object
			if (desiredType.IsSubclassOf(typeof(ReflectStub)))
			{
				var reflectedType = GetReflectedType(desiredType.AsType());
				if (desired.Position < 0)
					return reflectedType.IsAssignableFrom(candidateType);
				else
					return candidateType.IsAssignableFrom(reflectedType);
			}

			// Finally, we might be using an interface with less-derived types (i.e. object)
			return desired.Position < 0
				? desiredType.IsSubclassOf(candidateType)
				: candidateType.IsSubclassOf(desiredType);
		}

		// Finds a candidate method from a list
		private static T FindCandidateMethod<T>(T[] candidates, ParameterInfo[] args,
			out ParameterInfo[] foundArgs, ParameterInfo returnParam = null) where T : MethodBase
		{
			foreach (var m in candidates)
			{
				var mArgs = m.GetParameters();
				if (mArgs.Length != args.Length) continue;
				bool allValid = true;
				for (int i = 0; i < args.Length; i++)
					if (!ValidParameter(args[i], mArgs[i]))
					{
						allValid = false;
						break;
					}
				if (!allValid) continue;
				var methodInfo = m as MethodInfo;
				if (methodInfo != null && returnParam != null &&
					!ValidParameter(returnParam, methodInfo.ReturnParameter)) continue;
				foundArgs = mArgs;
				return m;
			}
			foundArgs = null;
			return null;
		}

		// Creates a casting expression from an input expression to a given target type
		private static Expression GetCastExpression(Expression e, Type to)
		{
			var targetType = to.GetTypeInfo();
			var eType = e.Type;
			if (to.IsByRef && !eType.IsByRef) eType = e.Type.GetTypeInfo().MakeByRefType();
			if (eType == to || targetType.IsAssignableFrom(eType.GetTypeInfo())) return e;
			if (targetType.IsSubclassOf(typeof(ReflectStub)))
				return Expression.New(GetStubCtor(to), e);
			if (eType.GetTypeInfo().IsSubclassOf(typeof(ReflectStub)) && targetType.IsAssignableFrom(GetReflectedType(eType)))
				e = Expression.Field(e, InstanceField);
			return Expression.Convert(e, to);
		}

		// Gets a constructor delegate from the given stub field
		private static Delegate GetCtor(FieldInfo field, Type stubType)
		{
			var rType = GetReflectedType(stubType);
			if(rType.IsAbstract) throw new Exception("abstract/static types cannot have constructor stubs");

			var invokeMethod = field.FieldType.GetTypeInfo().GetDeclaredMethod("Invoke");
			var args = invokeMethod.GetParameters();

			// Find a method that matches the exact signature
			var tc = GetTypeCache(stubType);
			var pTypes = args.Select(p => p.ParameterType).ToArray();
			var method = tc.Ctors.FirstOrDefault(c => c.GetParameters().Select(p => p.ParameterType).SequenceEqual(pTypes));
			var lambdaParams = args.Select(p => Expression.Parameter(p.ParameterType, p.Name)).ToArray();
			// ReSharper disable once CoVariantArrayConversion
			Expression[] exprArgs = lambdaParams;

			if (method == null)
			{
				method = FindCandidateMethod(GetTypeCache(stubType).Ctors, args, out var foundArgs);
				if(method == null)
					throw new Exception($"Unable to find candidate method for {field}");
				exprArgs = exprArgs.Select((e,i) => GetCastExpression(e, foundArgs[i].ParameterType)).ToArray();
			}
			
			var exprNew = Expression.New(method, exprArgs);
			var returnType = invokeMethod.ReturnType.GetTypeInfo();
			if (!returnType.IsAssignableFrom(exprNew.Type.GetTypeInfo()))
			{
				if(!returnType.IsAssignableFrom(stubType))
					throw new Exception($"Constructor stub {field} has an invalid return type");
				exprNew = Expression.New(GetStubCtor(stubType), exprNew);
			}
			return Expression.Lambda(field.FieldType, exprNew, lambdaParams).Compile();
		}

		// Currently it is not possible to create a dynamic method with a ref return, so this feature doesn't work
		// It can be re-enabled if and when this oversight is fixed
		/*
		private static ref T ReturnRef<T>(ref T input) => ref input;

		private static Func<object, Delegate> FieldDelegateCreator(FieldInfo field, Type stubType)
		{
			// Find a field with the right name
			var rType = GetReflectedType(stubType);
			// We assume that the given field is of type `FieldStub<T>`; this check should be done by the caller
			var fType = field.FieldType.GetTypeInfo().GenericTypeArguments[0];

			var candidate = rType.AsType().GetPrivateField(field.Name);
			if (fType != candidate.FieldType)
				throw new Exception($"The type of field access stub {field} must be exactly that of reflected field {candidate}");

			var returnRef = typeof(ReflectStub).GetTypeInfo().GetDeclaredMethod(nameof(ReturnRef)).MakeGenericMethod(fType);
			return o =>
			{
				Expression expr = Expression.Field(Expression.Constant(o, rType.AsType()), candidate);
				expr = Expression.Call(returnRef, expr);
				return Expression.Lambda(field.FieldType, expr).Compile();
			};
		}
		*/

		// Gets a delegate creator from a field stub.
		// Instance delegates may need to be created multiple times; by storing these creator methods we can skip a lot of the code the next time
		private static Func<object, Delegate> GetDelegateCreator(FieldInfo field, Type stubType)
		{
			var rType = GetReflectedType(stubType);
			var fType = field.FieldType;
			var invokeMethod = fType.GetTypeInfo().GetMethod("Invoke");
			var args = invokeMethod.GetParameters();

			// Find a method that matches the exact signature
			var tc = GetTypeCache(stubType);
			var methodList = field.IsStatic ? tc.StaticMethods : tc.InstanceMethods;
			var pTypes = args.Select(p => p.ParameterType).ToArray();
			var method = methodList.FirstOrDefault(m => m.Name == field.Name && m.GetParameters().Select(p => p.ParameterType).SequenceEqual(pTypes));

			// If the method exists, use it
			if (method != null && method.ReturnType == invokeMethod.ReturnType)
			{
				// ReSharper disable RedundantAssignment
				invokeMethod = null;
				rType = null;
				// ReSharper restore RedundantAssignment
				if (field.IsStatic)
					return o => method.CreateDelegate(fType);
				else
					return o => method.CreateDelegate(fType, o);
			}

			// Otherwise, find a compatible method and compile an expression
			method = FindCandidateMethod(methodList, args, out var foundArgs, invokeMethod.ReturnParameter);
			if(method == null)
				throw new Exception($"Unable to find candidate method for {field}");

			var exprArgs = args.Select(p => Expression.Parameter(p.ParameterType, p.Name)).ToArray();
			var exprCast = exprArgs.Select((e,i) => GetCastExpression(e, foundArgs[i].ParameterType)).ToArray();

#if NET35
			return o =>
			{
				Expression exprCall = method.IsStatic ? Expression.Call(method, exprCast) :
					Expression.Call(Expression.Constant(o, rType.AsType()), method, exprCast);
				if (!exprCall.Type.GetTypeInfo().IsAssignableFrom(invokeMethod.ReturnType.GetTypeInfo()))
				{
					if (GetReflectedType(invokeMethod.ReturnType, false)?.IsAssignableFrom(exprCall.Type.GetTypeInfo()) == true)
					{
						exprCall = Expression.New(GetStubCtor(invokeMethod.ReturnType), exprCall);
					}
					else
						exprCall = Expression.Convert(exprCall, invokeMethod.ReturnType);
				}
				return Expression.Lambda(fType, exprCall, exprArgs).Compile();
			};
#else

			// Assign input arguments to variables of the correct type
			var variables = foundArgs.Select((p, i) =>
					Expression.Variable(p.ParameterType.IsByRef ? p.ParameterType.GetElementType() : p.ParameterType, "_" + p.Name)).ToArray();
			var assignments = variables.Select((v, i) => Expression.Assign(v, exprCast[i]));
			return o =>
			{
				// Call the method
				// ReSharper disable CoVariantArrayConversion
				Expression exprCall = method.IsStatic ? Expression.Call(method, variables) :
					Expression.Call(Expression.Constant(o, rType.AsType()), method, variables);
				// ReSharper restore CoVariantArrayConversion

				// Cast the arguments back to the inputs
				var assignBack = exprArgs.Where(a => a.IsByRef)
					.Select(a => Expression.Assign(a, GetCastExpression(variables[Array.IndexOf(exprArgs, a)], a.Type)));
				var castReturn = GetCastExpression(exprCall, invokeMethod.ReturnType);

				return Expression.Lambda(fType,
					Expression.Block(variables, assignments.Concat(new[] {exprCall}).Concat(assignBack)
						.Concat(new[] {castReturn})), exprArgs).Compile();
			};
#endif
		}

		// Gets a delegate from a field stub, using a cached creator if it exists
		private static Delegate GetDelegate(FieldInfo field, Type stubType, object instance)
		{
			var tc = GetTypeCache(stubType);
			if (!tc.Creators.TryGetValue(field, out var creator))
			{
				// Currently it is not possible to create a dynamic method with a ref return, so this feature doesn't work
				// It can be re-enabled if and when this oversight is fixed
				/*bool isFieldAccess = field.FieldType.IsConstructedGenericType &&
								 field.FieldType.GetGenericTypeDefinition() == typeof(FieldStub<>);*/
				creator = /*isFieldAccess ? FieldDelegateCreator(field, stubType) :*/ GetDelegateCreator(field, stubType);
				if(!field.IsStatic)
					tc.Creators.Add(field, creator);
			}
			return creator(field.IsStatic ? null : instance);
		}
	}
}
