bob = Object ~ {
    name: "Bob",
    greet: fn(self, name) _ {
        puts("Hello " + name + ", I'm " + self.name);
    }
};

bob.greet(bob, "Martha");