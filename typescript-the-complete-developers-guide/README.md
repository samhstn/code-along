# Typescript - the complete developers guide

See: https://www.udemy.com/share/101WXkAEcaeVhWTXoF

### Environment setup

```bash
npm install -g typescript ts-node
```

To compile and execute typescript code, run:

```bash
ts-node index.ts
```

Type - An easy way to refer to the different properties and functions that a value has.

### Annotations

We should always rely on type annotations, except when:
1. When a function returns the `any` type.
```typescript
const json = '{"x": 10, "y": 20}';
const coordinates = JSON.parse(json);
console.log(coordinates); // {x: 10, y: 20}
```
`JSON.parse` returns the `any` type, here we should add types to coordinates.

Avoid variables with `any` at all costs!

2. When we declare a variable on one line and initialize it later.

```typescript
let words = ['red', 'green', 'blue'];
let foundWord: boolean;

for (let i = 0; i < words.length; i++) {
  if (words[i] === 'green') {
    foundWord = true;
  }
}
```

3. Variable whose type cannot be inferred correctly
```typescript
let numbers = [-10, -1, 12];
let numberAboveZero: boolean | number = false;

for (let i = 0; i < numbers.length; i++) {
  if (numbers[i] > 0) {
    numberAboveZero = numbers[i];
  }
}
```

Typed arrays - Arrays where each element is some consistent type of value.

Typle - Array-like structure where each element represents some property of a record.

Class functions come in 3 forms:

1. public (default) - This method can be called anywhere, any time.
2. private - This method can only be called by other methods in this class.
3. protected - This method can be called by other methods in this class, or by other methods in the child classes.
