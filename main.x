hello = "outside";
print "outer 1: " + hello;
{
    print "inner 1: " + hello;
    hello = "inside 2";
    print "inner 2: " + hello;
};
print "outer 2: " + hello;
{
    print "inner 3: " + hello;
    nonlocal hello = "inside 2";
    print "inner 4: " + hello;
};
print "outer 3: " + hello;