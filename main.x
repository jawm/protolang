x = 10;

abc = fn () {
    nonlocal x = 5;
};

abc();

print x;