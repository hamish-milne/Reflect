using System;
using System.Collections.Generic;
using System.Linq;
using System.Linq.Expressions;
using System.Reflection;

namespace Reflect
{
#if !NETSTANDARD
	using TypeInfo = System.Type;
	internal static class TypeInfoEx
	{
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

		public static ConstructorInfo[] GetDeclaredConstructors(this Type type)
			=> type.GetConstructors(BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance);

		public static IEnumerable<MethodInfo> GetRuntimeMethods(this Type type)
			=> type.GetMethods(BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Static);

		public static object CreateInstance(this Type type)
			=> Activator.CreateInstance(type, true);
	}
#endif

#if NETSTANDARD

	internal static class TypeInfoEx
	{
		public static FieldInfo GetField(this TypeInfo type, string name)
			=> type.GetDeclaredField(name);

		public static ConstructorInfo GetConstructor(this TypeInfo type, Type[] arguments)
			=> type.DeclaredConstructors
			.FirstOrDefault(c => c.GetParameters().Select(p => p.ParameterType)
				.SequenceEqual(arguments));

		public static MethodInfo GetMethod(this TypeInfo type, string name, Type[] parameters)
			=> type.AsType().GetRuntimeMethod(name, parameters);

		public static MethodInfo GetMethod(this TypeInfo type, string name)
			=> type.GetDeclaredMethod(name);

		public static IEnumerable<FieldInfo> GetFields(this Type type)
			=> type.GetRuntimeFields();

		public static bool IsInstanceOfType(this TypeInfo type, object obj)
			=> type.IsAssignableFrom(obj.GetType().GetTypeInfo());

		public static ConstructorInfo[] GetDeclaredConstructors(this TypeInfo type)
			=> type.DeclaredConstructors.ToArray();

		public static object CreateInstance(this TypeInfo type)
			=> type.GetConstructor(new Type[0]).Invoke((object[])null);
	}

#endif

	/// <summary>
	/// Apply to a method stub to look for a constructor rather than a method
	/// </summary>
	[AttributeUsage(AttributeTargets.Field)]
	public class ConstructorAttribute : Attribute { }

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
	public abstract class Reflect
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
				InstanceMethods = type.AsType().GetRuntimeMethods().Where(m => !m.IsAbstract && !m.IsStatic).ToArray();
				StaticMethods = type.AsType().GetRuntimeMethods().Where(m => !m.IsAbstract && m.IsStatic).ToArray();
			}
		}

		// Map of reflected types to type caches
		private static readonly Dictionary<TypeInfo, TypeCache> Cache = new Dictionary<TypeInfo, TypeCache>();

		// Gets a type cache for the reflected type
		private TypeCache GetTypeCache()
		{
			if(!Cache.TryGetValue(_type, out var tc))
				Cache.Add(_type, tc = new TypeCache(_type));
			return tc;
		}

		// Map of reflected types to method stub fields
		private static readonly Dictionary<Type, FieldInfo[]> ValidFieldsCache = new Dictionary<Type, FieldInfo[]>();

		// The reflected instance
		private readonly object _instance;
		// The reflected type
		private readonly TypeInfo _type;
		// The stub type's constructor, for reflection interop
		private readonly ConstructorInfo _ctor;

		// The `_instance` field, for reflection interop
		private static readonly FieldInfo InstanceField =
			typeof(Reflect).GetTypeInfo().GetField(nameof(_instance));

		/// <summary>
		/// Gets the reflected instance for the given stub instance
		/// </summary>
		/// <param name="r">The stub instance, or <c>null</c></param>
		/// <returns>The reflected instance, or <c>null</c></returns>
		public static object GetInstance(Reflect r) => r?._instance;

		/// <summary>
		/// Gets the reflected type for the given stub instance
		/// </summary>
		/// <param name="r">The stub instance, or <c>null</c></param>
		/// <returns>The reflected type, or <c>null</c></returns>
		public static TypeInfo GetType(Reflect r) => r?._type;

		/// <summary>
		/// Gets the reflected type for the given stub type
		/// </summary>
		/// <param name="rType">The stub type</param>
		/// <param name="exception">If <c>true</c>, throw an exception if a reflected type could not be found</param>
		/// <returns>The reflected type</returns>
		public static TypeInfo GetType(TypeInfo rType, bool exception = true)
		{
			var attr = rType.GetCustomAttribute<ReflectedTypeAttribute>();
			if (attr == null)
			{
				if (exception) throw new Exception($"Reflect type {rType} has no ReflectedTypeAttribute");
				return null;
			}
			var type = attr.Type ?? GetType(attr.TypeName, attr.AssemblyType?.GetTypeInfo().Assembly);
			if(type == null && exception) throw new Exception($"Attribute for {rType} does not define reflected type");
			return type.GetTypeInfo();
		}
		
		// Gets a type by name
		private static Type GetType(string type, Assembly assembly)
		{
			if (assembly != null)
				return assembly.GetType(type);
#if NETSTANDARD
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
		protected Reflect(object instance = null)
		{
			_type = GetType(GetType().GetTypeInfo());
			_instance = instance ?? (_type.IsAbstract ? null : _type.CreateInstance());
			_ctor = GetType().GetTypeInfo().GetConstructor(new[] { typeof(object) });
			if(!_type.IsInstanceOfType(_instance))
				throw new Exception($"The given object {_instance} is not an instance of reflected type {_type}");
			Initialize();
		}

		/// <summary>
		/// Reflect stubs will compare equal where their instances (or types, where they're abstract) are equal
		/// </summary>
		/// <param name="obj"></param>
		/// <returns></returns>
		public override bool Equals(object obj)
		{
			if (ReferenceEquals(this, obj)) return true;
			var other = obj as Reflect;
			if (other == null || (other._instance == null) != (_instance == null)) return false;
			if (_instance == null)
				return other._type == _type;
			return _instance.Equals(other._instance);
		}

		/// <summary>
		/// Gets the reflected instance or type hashcode
		/// </summary>
		/// <returns></returns>
		public override int GetHashCode()
		{
			return (_instance ?? _type).GetHashCode();
		}

		// Sets the values of all stub fields
		private void Initialize()
		{
			if (!ValidFieldsCache.TryGetValue(GetType(), out var fields))
			{
				fields = GetType().GetRuntimeFields()
					.Where(f => f.DeclaringType != null && f.DeclaringType.GetTypeInfo().IsSubclassOf(typeof(Reflect)))
					.Where(f => f.FieldType.GetTypeInfo().IsSubclassOf(typeof(Delegate)))
					.ToArray();
				ValidFieldsCache.Add(GetType(), fields);
			}
			foreach (var f in fields)
			{
				if (f.GetValue(this) != null) continue;
				var del = f.IsDefined(typeof(ConstructorAttribute)) ? GetCtor(f) : GetDelegate(f);
				f.SetValue(this, del);
			}
		}

		// Checks if a candidate parameter matches the desired stub one
		private static bool ValidParameter(ParameterInfo desired, ParameterInfo candidate)
		{
			var desiredType = desired.ParameterType.GetTypeInfo();
			var candidateType = candidate.ParameterType.GetTypeInfo();

			// Check for direct assignment capability
			var isAssignable = desired.Position < 0
				? desiredType.IsAssignableFrom(candidateType)
				: candidateType.IsAssignableFrom(desiredType);
			if (isAssignable)
				return true;

/*#if NETSTANDARD
			// For the case of 'ref' or 'out' parameters
			if (candidate.ParameterType.IsByRef)
			{
				if (!desired.ParameterType.IsByRef) return false;
				if (desired.IsOut != candidate.IsOut) return false;
				// We've already checked the simple case where both types are public
				// so we can assume that the desired type is a Reflect subclass
				var tDesired = GetType(desired.ParameterType.GetElementType().GetTypeInfo(), false);
				var tCandidate = candidate.ParameterType.GetElementType().GetTypeInfo();
				if (tDesired == null || tCandidate == null) return false;
				// For 'out' parameters, the invokee can be a subclass
				if (desired.IsOut)
					return tDesired.IsAssignableFrom(tCandidate);
				// for 'ref' parameters, they need to be identical
				return tDesired.AsType() == tCandidate.AsType();
			}
#endif*/

			// It might also be valid if we're passing in a Reflect object
			if (!desiredType.IsSubclassOf(typeof(Reflect))) return false;
			var reflectedType = GetType(desiredType);
			if (desired.Position < 0)
				return reflectedType.IsAssignableFrom(candidateType);
			else
				return candidateType.IsAssignableFrom(reflectedType);
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

		// Creates a casting expression from an input, given a parameter
		private static Expression GetCastExpression(Expression e, ParameterInfo arg)
		{
			var targetType = arg.ParameterType;
			var eType = arg.ParameterType.IsByRef ? e.Type.MakeByRefType() : e.Type; 
			if (eType == targetType) return e;
			if (eType.GetTypeInfo().IsSubclassOf(typeof(Reflect)))
				e = Expression.Field(e, InstanceField);
			if (eType.GetTypeInfo().IsAssignableFrom(targetType.GetTypeInfo())) return e;
			return Expression.Convert(e, targetType);
		}

		// Gets a constructor delegate from the given stub field
		private Delegate GetCtor(FieldInfo field)
		{
			var invokeMethod = field.FieldType.GetTypeInfo().GetDeclaredMethod("Invoke");
			var args = invokeMethod.GetParameters();

			// Find a method that matches the exact signature
			var method = _type.GetConstructor(args.Select(p => p.ParameterType).ToArray());
			var exprArgs = args.Select(p => Expression.Parameter(p.ParameterType, p.Name)).Cast<Expression>().ToArray();

			if (method == null)
			{
				method = FindCandidateMethod(GetTypeCache().Ctors, args, out var foundArgs);
				if(method == null)
					throw new Exception($"Unable to find candidate method for {field}");
				exprArgs = exprArgs.Select((e,i) => GetCastExpression(e, foundArgs[i])).ToArray();
			}

			// ReSharper disable once RedundantCast
			var exprNew = Expression.New(method, (Expression[])exprArgs);
			var returnType = invokeMethod.ReturnType.GetTypeInfo();
			if (!returnType.IsAssignableFrom(exprNew.Type.GetTypeInfo()))
			{
				if(!returnType.IsAssignableFrom(_type))
					throw new Exception($"Constructor stub {field} has an invalid return type");
				if (_ctor == null)
					throw new Exception($"The Reflect type {GetType()} has no constructor taking an object instance argument");
				exprNew = Expression.New(_ctor, exprNew);
			}
			return Expression.Lambda(field.FieldType, exprNew).Compile();
		}

		// Gets a delegate creator from a field stub.
		// Instance delegates may need to be created multiple times; by storing these creator methods we can skip a lot of the code the next time
		private Func<object, Delegate> GetDelegateCreator(FieldInfo field)
		{
			var invokeMethod = field.FieldType.GetTypeInfo().GetMethod("Invoke");
			var args = invokeMethod.GetParameters();

			// Find a method that matches the exact signature
			var method = _type.GetMethod(field.Name, args.Select(p => p.ParameterType).ToArray());

			// If the method exists, use it
			if (method != null && method.ReturnType == invokeMethod.ReturnType)
			{
				if (field.IsStatic)
					return o => method.CreateDelegate(field.FieldType);
				else
					return o => method.CreateDelegate(field.FieldType, o);
			}

			// Otherwise, find a compatible method and compile an expression
			var tc = GetTypeCache();
			method = FindCandidateMethod(field.IsStatic ? tc.StaticMethods : tc.InstanceMethods, args, out var foundArgs, invokeMethod.ReturnParameter);
			if(method == null)
				throw new Exception($"Unable to find candidate method for {field}");

			var exprArgs = args.Select(p => Expression.Parameter(p.ParameterType, p.Name)).ToArray();
			var exprCast = exprArgs.Select((e,i) => GetCastExpression(e, foundArgs[i])).ToArray();

			return o =>
			{
				Expression exprCall = method.IsStatic ? Expression.Call(method, exprCast) :
					Expression.Call(Expression.Constant(o, _type.AsType()), method, exprCast);
				if (!exprCall.Type.GetTypeInfo().IsAssignableFrom(invokeMethod.ReturnType.GetTypeInfo()))
				{
					if (GetType(invokeMethod.ReturnType.GetTypeInfo(), false)?.IsAssignableFrom(exprCall.Type.GetTypeInfo()) == true)
					{
						if (_ctor == null)
							throw new Exception($"The Reflect type {GetType()} has no constructor taking an object instance argument");
						exprCall = Expression.New(_ctor, exprCall);
					}
					else
						exprCall = Expression.Convert(exprCall, invokeMethod.ReturnType);
				}
				return Expression.Lambda(field.FieldType, exprCall, exprArgs).Compile();
			};
		}

		// Gets a delegate from a field stub, using a cached creator if it exists
		private Delegate GetDelegate(FieldInfo field)
		{
			var tc = GetTypeCache();
			if (!tc.Creators.TryGetValue(field, out var creator))
			{
				creator = GetDelegateCreator(field);
				if(!field.IsStatic)
					tc.Creators.Add(field, creator);
			}
			return creator(field.IsStatic ? null : _instance);
		}
	}
}
