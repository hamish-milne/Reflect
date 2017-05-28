using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Reflect;

namespace ReflectTest
{
	internal class FooClass
	{
		internal void Foo(int a, BarClass bar)
		{
			Console.WriteLine($"{a} {bar.Bar(a)}");
		}

		internal void Baz(out object baz)
		{
			baz = new BarClass();
		}

		internal static FooClass Property { get; } = new FooClass();
	}

	internal class BarClass
	{
		internal int Bar(int b)
		{
			return b + 1;
		}
	}

	public delegate void OutVar<T>(out T value);

	[ReflectedType("ReflectTest.FooClass")]
	public class RFooClass : Reflect.Reflect
	{
		public readonly Action<int, RBarClass> Foo;
		public readonly OutVar<object> Baz;
		public static readonly Func<RFooClass> get_Property;

		public static RFooClass _Property => get_Property();

		public RFooClass(object instance = null) : base(instance) { }
	}

	[ReflectedType("ReflectTest.BarClass")]
	public class RBarClass : Reflect.Reflect
	{
		public readonly Func<int, int> Bar;
	}

	class Program
	{
		static void Main(string[] args)
		{
			var bar = new RBarClass();
			var foo = new RFooClass();
			var t1 = bar.Bar(5);
			foo.Foo(5, bar);
			foo.Baz(out var baz);
			var p = RFooClass._Property;

			var d = new Dictionary<int, RBarClass>();
		}
	}
}
