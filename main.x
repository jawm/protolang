start = clock();
fibonacci = fn(number) {
    if (number == 0)
        0
    else if (number == 1)
        1
    else fibonacci(number-1) + fibonacci(number-2)
};

puts(fibonacci(20));
end = clock();
puts(end - start);