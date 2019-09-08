scrambled = Object ~ {
    with: fn()_ puts("scrambled")
};

egg = Object ~ {
    scramble: fn()_ scrambled
};

egg.scramble().with();

Person = Object ~ {
    proto greet: fn(self, other) _ puts("Hello " + other.name + ", I'm " + self.name + ".")
};

bob = Person ~ {
    name: "Bob",
};

martha = Person ~ {
    name: "Martha",
};

jesse = Person ~ {
    name: "Jesse",
    greet: fn(self, other) _ puts("Yo wassup " + other.name + ", the name's " + self.name),
};

bob:greet(martha);
martha:greet(bob);
bob:greet(jesse);
jesse:greet(bob);

hello = Object ~ {};
hello.what = "hello, loser";
puts(hello.what);