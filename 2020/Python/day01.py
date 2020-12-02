def solve_p2():
    numbers = []
    with open("input.txt") as f:
        for line in f:
            num = int(line)
            numbers.append(num)
    combos = [((a, b, c), a*b*c) if a+b+c == 2020 else None for (a, b, c) in itertools.combinations(numbers, 3)]
    for combo in combos:
        if combo is not None:
            print(combo)

solve_p2()
