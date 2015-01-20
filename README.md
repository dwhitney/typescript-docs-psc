# typescript-docs

A documentation generator for TypeScript code

## Installation

Install globally using `npm install -g typescript-docs`.

## Usage

`typescript-docs` works on TypeScript definition (`.d.ts`) files to create HTML documentation.

Generate a `.d.ts` file for your TypeScript code, by using the `-d` flag on the command line, and then pass the resulting file to `typescript-docs` as follows:

```
typescript-docs -i my-defs.d.ts
```

This will print the HTML onto standard output. To redirect the HTML output to a file, use the `-o` flag:

```
typescript-docs -i my-defs.d.ts -o index.html
```

## Contributing

If you would like to contribute to this project, you will need to install the [PureScript compiler](http://purescript.org).

Use Bower to pull dependencies, and [Pulp](https://github.com/bodil/pulp) to build the sources via the bundled Makefile:

```
bower update
npm install -g pulp
make
```
