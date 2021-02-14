class Vehicle {
  constructor(public color: string) {}

  drive(): void {
    console.log('chugga chugga');
  }

  honk(): void {
    console.log('beep');
  }
}

class Car extends Vehicle {
  constructor(public wheels: number, color: string) {
    super(color);
  }
  drive(): void {
    console.log('vroom');
  }
}

const car = new Car(4, 'red');
car.drive();
car.honk();
