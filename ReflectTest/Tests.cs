using System;
using Reflect;
using NUnit.Framework;
// ReSharper disable UnusedMember.Local
// ReSharper disable ClassNeverInstantiated.Local
#pragma warning disable 649

namespace ReflectTest
{
	[TestFixture]
	public class Tests
	{
		delegate void RefSingle<T>(ref T a);

		// -------------------------------------------------
		// Instance method

		internal class InstanceMethodClass
		{
			private int Foo(int a, int b) => a + b;
		}

		[ReflectedType(typeof(InstanceMethodClass))]
		internal class RInstanceMethodClass : ReflectStub
		{
			public readonly Func<int, int, int> Foo;
		}

		[Test]
		public void InstanceMethod()
		{
			var o = new RInstanceMethodClass();
			Assert.AreEqual(5, o.Foo(2, 3));
		}

		// -------------------------------------------------
		// Static method

		internal static class StaticMethodClass
		{
			private static int Foo(int a, int b) => a + b;
		}

		[ReflectedType(typeof(StaticMethodClass))]
		abstract class RStaticMethodClass : ReflectStub
		{
			public static readonly Func<int, int, int> Foo;

			static RStaticMethodClass() => InitializeStatic(typeof(RStaticMethodClass));
		}

		[Test]
		public void StaticMethod()
		{
			Assert.AreEqual(5, RStaticMethodClass.Foo(2, 3));
		}

		// -------------------------------------------------
		// Interface

		internal interface IInterfaceTest
		{
			int Foo(int a, int b);
		}

		internal class InterfaceTestImpl : IInterfaceTest
		{
			public int Foo(int a, int b) => a + b;
		}

		internal static class InterfaceTestFactory
		{
			private static IInterfaceTest Create() => new InterfaceTestImpl();
		}

		[ReflectedType(typeof(IInterfaceTest))]
		class RInterfaceTest : ReflectStub
		{
			public RInterfaceTest(object instance) : base(instance) { }

			public readonly Func<int, int, int> Foo;
		}

		[ReflectedType(typeof(InterfaceTestFactory))]
		abstract class RInterfaceTestFactory : ReflectStub
		{
			public static readonly Func<RInterfaceTest> Create;

			static RInterfaceTestFactory() => InitializeStatic(typeof(RInterfaceTestFactory));
		}

		[Test]
		public void Interface()
		{
			var iface = RInterfaceTestFactory.Create();
			Assert.AreEqual(5, iface.Foo(2, 3));
		}
		// -------------------------------------------------
		// Field

		// Currently it is not possible to create a dynamic method with a ref return, so this feature doesn't work
		// It can be re-enabled if and when this oversight is fixed
		/*
		internal class FieldTestClass
		{
			private int foo = 2;
		}

		[ReflectedType(typeof(FieldTestClass))]
		class RFieldTestClass : ReflectStub
		{
			public readonly FieldStub<int> foo;
		}

		[Test]
		public void Field()
		{
			var o = new RFieldTestClass();
			ref var foo = ref o.foo();
			foo += 3;
			Assert.AreEqual(5, foo);
		}
		*/


		// -------------------------------------------------
		// Ref primitive

		internal class RefPrimitiveClass
		{
			private void Foo(ref int a) => a++;
		}

		[ReflectedType(typeof(RefPrimitiveClass))]
		class RRefPrimitiveClass : ReflectStub
		{
			public readonly RefSingle<int> Foo;
		}

		[Test]
		public void RefPrimitive()
		{
			var o = new RRefPrimitiveClass();
			var a = 3;
			o.Foo(ref a);
			Assert.AreEqual(4, a);
		}
		
		// -------------------------------------------------
		// Reflect stub transformation

		internal class StubTransformClass
		{
			public readonly int Foo;

			internal StubTransformClass(int foo)
			{
				Foo = foo;
			}

			private static StubTransformClass Bar(StubTransformClass a, StubTransformClass b)
			{
				return new StubTransformClass(a.Foo + b.Foo);
			}
		}

		[ReflectedType(typeof(StubTransformClass))]
		class RStubTransformClass : ReflectStub
		{
			public RStubTransformClass(object instance) : base(instance) { }
			static RStubTransformClass() => InitializeStatic(typeof(RStubTransformClass));

			public static readonly Func<RStubTransformClass, RStubTransformClass, RStubTransformClass> Bar;

			[Constructor] public static readonly Func<int, RStubTransformClass> Ctor;
		}

		[Test]
		public void StubTransform()
		{
			var a = RStubTransformClass.Ctor(2);
			var b = RStubTransformClass.Ctor(3);
			var c = RStubTransformClass.Bar(a, b);
			Assert.AreEqual(5, ((StubTransformClass)ReflectStub.GetInstance(c)).Foo);
		}

		// -------------------------------------------------
		// Object cast


		[ReflectedType(typeof(StubTransformClass))]
		class RObjectCastClass : ReflectStub
		{
			public RObjectCastClass(object instance) : base(instance) { }
			static RObjectCastClass() => InitializeStatic();

			public static readonly Func<object, object, object> Bar;
		}

		[Test]
		public void ObjectCast()
		{
			var a = new StubTransformClass(2);
			var b = new StubTransformClass(3);
			var c = RObjectCastClass.Bar(a, b);
			Assert.AreEqual(5, ((StubTransformClass)c).Foo);
		}


		// -------------------------------------------------
		// Property

		class PropertyTestClass
		{
			private int Foo { get; set; } = 2;
		}

		[ReflectedType(typeof(PropertyTestClass))]
		class RPropertyTestClass : ReflectStub
		{
			// ReSharper disable InconsistentNaming
			private readonly Func<int> get_Foo;
			private readonly Action<int> set_Foo;
			// ReSharper restore InconsistentNaming

			public int PFoo
			{
				get => get_Foo();
				set => set_Foo(value);
			}
		}

		[Test]
		public void Property()
		{
			var o = new RPropertyTestClass();
			o.PFoo += 3;
			Assert.AreEqual(5, o.PFoo);
		}
	}
}
