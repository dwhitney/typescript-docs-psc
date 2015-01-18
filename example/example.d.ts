/**
* An example of a module
*/
declare module ExampleModule {
    /**
    * A nested module
    */
    module AnotherModule {
        /**
        * An example of a class
        */
        class Foo implements Bar {
            /**
            * Constructor example
            */
            constructor();
            public getFoo(name: string, optionalParameter?: number): Foo;
        }
        /**
        * An example of an interface
        */
        interface Bar {
            /**
            * Get the Foo corresponding to this Bar
            *
            * @param { name } The name
            */
            getFoo(name: string, optionalParameter?: number): Foo;
        }

        // An interface with a varargs method
        interface Length {
        
            lengthOf<T>(...values: T[]);
        }
    }
    /**
    * Generics are supported too
    */
    function makeFoo<T1, T2>(options?: {
        name: string;
        t1: T1;
        t2?: T2;
    }): AnotherModule.Foo;
    /**
    * An example of a variable
    */
    var globalVariable: string;
    /**
    * An enumeration
    */
    enum Baz {
        Abc,
        Xyz,
    }
}