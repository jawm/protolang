# here we define variable `abc` at the top level.
abc = "hello";

# If we print it, the output will be 'hello' as we'd expect;
print abc;

# We will now create a block of code. This has it's own scope for variables.
{
    # We can still access the variable since it iterates up through parent scopes until finding a matching name
    print abc + ", world";

    # We have now overridden with a new value. This is only accessible from the current scope.
    abc = "defined inside block";
    print abc;
};

# If we print the variable again, we can see that the value defined inside the block didn't affect the outer definition
print "outside block: " + abc;

# Sometimes though, we _do_ want to alter a variable from a higher scope. To do that we have the nonlocal keyword
{
    # The nonlocal keyword will look through all parent scopes until it finds a variable that matches, and will modify
    # that one.
    nonlocal abc = "defined inside another block";
    print abc;
};

# We can see that the variable will hold the value as defined inside the block this time.
print abc;

# We also now have if/else statements, and logical operators

if (true) print "it was true";
if (false) print "this won't be printed" else print "but this will be";

# Since a block is just a normal expression, we can use them to build up more complex if/else constructs:
variable = "knock knock";
if (6 * 7 == 42) {
    nonlocal variable = "who's there?";
    print "this branch will execute";
} else {
    print "this branch won't execute";
};
print variable;

# Since if/else is just an expression, we can use it like a ternary operator:
conditionalVariable = if (true) 15 else 16;
print conditionalVariable;

# The final change is that I've reduced the amount of semicolons needed.
# The final line in your input is allowed to skip the semicolon, as is the final line in a block.
# Blocks themselves, and if statements still need to have semicolons at the end, since they are just a normal expression.

# While loops:

