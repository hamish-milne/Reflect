using System;
using System.Collections.Generic;
using System.Linq;
using System.Linq.Expressions;
using System.Reflection;

namespace Reflect
{
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

		public ReflectedTypeAttribute(Type type) => Type = type;
		public ReflectedTypeAttribute(string typeName) => TypeName = typeName;
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

			public TypeCache(Type type)
			{
				const BindingFlags bf = BindingFlags.Public | BindingFlags.NonPublic;
				Ctors = type.GetConstructors(bf | BindingFlags.Instance);
				InstanceMethods = type.GetMethods(bf | BindingFlags.Instance);
				StaticMethods = type.GetMethods(bf | BindingFlags.Static);
			}
		}

		// Map of reflected types to type caches
		private static readonly Dictionary<Type, TypeCache> Cache = new Dictionary<Type, TypeCache>();

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
		private readonly Type _type;
		// The stub type's constructor, for reflection interop
		private readonly ConstructorInfo _ctor;

		// The `_instance` field, for reflection interop
		private static readonly FieldInfo InstanceField =
			typeof(Reflect).GetField(nameof(_instance), BindingFlags.Instance | BindingFlags.NonPublic);

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
		public static Type GetType(Reflect r) => r?._type;

		/// <summary>
		/// Gets the reflected type for the given stub type
		/// </summary>
		/// <param name="rType">The stub type</param>
		/// <param name="exception">If <c>true</c>, throw an exception if a reflected type could not be found</param>
		/// <returns>The reflected type</returns>
		public static Type GetType(Type rType, bool exception = true)
		{
			var attr = (ReflectedTypeAttribute)Attribute.GetCustomAttribute(rType, typeof(ReflectedTypeAttribute));
			if (attr == null)
			{
				if (exception) throw new Exception($"Reflect type {rType} has no ReflectedTypeAttribute");
				return null;
			}
			var type = attr.Type ?? GetType(attr.TypeName);
			if(type == null && exception) throw new Exception($"Attribute for {rType} does not define reflected type");
			return type;
		}

		// Gets a type by name
		private static Type GetType(string type)
		{
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
		}

		/// <summary>
		/// Creates a new instance, optionally passing in an existing reflected instance
		/// </summary>
		/// <param name="instance">An existing instance, or <c>null</c> to create a new one</param>
		protected Reflect(object instance = null)
		{
			_type = GetType(GetType());
			_instance = instance ?? (_type.IsAbstract ? null : Activator.CreateInstance(_type, true));
			_ctor = GetType().GetConstructor(BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance, null, new[] {typeof(object)}, null);
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
				fields = GetType().GetFields(
						BindingFlags.Instance |
						BindingFlags.Static |
						BindingFlags.Public |
						BindingFlags.NonPublic)
					.Where(f => f.DeclaringType != null && f.DeclaringType.IsSubclassOf(typeof(Reflect)))
					.Where(f => f.FieldType.IsSubclassOf(typeof(Delegate)))
					.ToArray();
				ValidFieldsCache.Add(GetType(), fields);
			}
			foreach (var f in fields)
			{
				if (f.GetValue(this) != null) continue;
				var del = Attribute.IsDefined(f, typeof(ConstructorAttribute)) ? GetCtor(f) : GetDelegate(f);
				f.SetValue(this, del);
			}
		}

		// Checks if a candidate parameter matches the desired stub one
		private static bool ValidParameter(ParameterInfo desired, ParameterInfo candidate)
		{
			// Check for direct assignment capability
			var isAssignable = desired.Position < 0
				? desired.ParameterType.IsAssignableFrom(candidate.ParameterType)
				: candidate.ParameterType.IsAssignableFrom(desired.ParameterType);
			if (isAssignable)
				return true;

#if NET4
			// For the case of 'ref' or 'out' parameters
			if (candidate.ParameterType.IsByRef)
			{
				if (!desired.ParameterType.IsByRef) return false;
				if (desired.IsOut != candidate.IsOut) return false;
				// We've already checked the simple case where both types are public
				// so we can assume that the desired type is a Reflect subclass
				var tDesired = GetType(desired.ParameterType.GetElementType(), false);
				var tCandidate = candidate.ParameterType.GetElementType();
				if (tDesired == null || tCandidate == null) return false;
				// For 'out' parameters, the invokee can be a subclass
				if (desired.IsOut)
					return tDesired.IsAssignableFrom(tCandidate);
				// for 'ref' parameters, they need to be identical
				return tDesired == tCandidate;
			}
#endif

			// It might also be valid if we're passing in a Reflect object
			if (!desired.ParameterType.IsSubclassOf(typeof(Reflect))) return false;
			var reflectedType = GetType(desired.ParameterType);
			if (desired.Position < 0)
				return reflectedType.IsAssignableFrom(candidate.ParameterType);
			else
				return candidate.ParameterType.IsAssignableFrom(reflectedType);
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
			if (e.Type == targetType) return e;
			if (e.Type.IsSubclassOf(typeof(Reflect)))
				e = Expression.Field(e, InstanceField);
			if (targetType.IsAssignableFrom(e.Type)) return e;
			return Expression.Convert(e, targetType);
		}

		// Gets a constructor delegate from the given stub field
		private Delegate GetCtor(FieldInfo field)
		{
			var invokeMethod = field.FieldType.GetMethod("Invoke");
			var args = invokeMethod.GetParameters();
			var bf = BindingFlags.Public | BindingFlags.NonPublic;

			// Find a method that matches the exact signature
			var method = _type.GetConstructor(bf, null, args.Select(p => p.ParameterType).ToArray(), null);
			var exprArgs = args.Select(p => Expression.Parameter(p.ParameterType, p.Name)).Cast<Expression>().ToArray();

			if (method == null)
			{
				method = FindCandidateMethod(GetTypeCache().Ctors, args, out var foundArgs);
				if(method == null)
					throw new Exception($"Unable to find candidate method for {field}");
				exprArgs = exprArgs.Select((e,i) => GetCastExpression(e, foundArgs[i])).ToArray();
			}

			var exprNew = Expression.New(method, exprArgs);
			if (!invokeMethod.ReturnType.IsAssignableFrom(exprNew.Type))
			{
				if(!invokeMethod.ReturnType.IsAssignableFrom(_type))
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
			var invokeMethod = field.FieldType.GetMethod("Invoke");
			var args = invokeMethod.GetParameters();
			var bf = BindingFlags.Public | BindingFlags.NonPublic;
			bf |= field.IsStatic ? BindingFlags.Static : BindingFlags.Instance;

			// Find a method that matches the exact signature
			var method = _type.GetMethod(field.Name, bf, null, args.Select(p => p.ParameterType).ToArray(), null);

			// If the method exists, use it
			if (method != null && method.ReturnType == invokeMethod.ReturnType)
			{
				if(field.IsStatic)
					return o => Delegate.CreateDelegate(field.FieldType, method, true);
				else
					return o => Delegate.CreateDelegate(field.FieldType, o, method, true);
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
					Expression.Call(Expression.Constant(o, _type), method, exprCast);
				if (!exprCall.Type.IsAssignableFrom(invokeMethod.ReturnType))
				{
					if (GetType(invokeMethod.ReturnType, false)?.IsAssignableFrom(exprCall.Type) == true)
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
