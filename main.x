hello = "outside";
abc = 1;
print ({
    hello = "inside";
    print "inner: " + hello;
    {
        hmm = "what";
        print hmm;
    };
    print abc;
    1 + 2;
});
print "outer: " + hello;