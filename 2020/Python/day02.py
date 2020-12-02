def part1():
    result = 0
    with open("input2.txt") as f:
        for line in f:
            (policy, (char,_), passcode) = line.split()
            min_p, max_p = policy.split("-")
            number_of_char = passcode.count(char)
            if int(min_p) <= number_of_char <= int(max_p):
                result += 1
        print(result)


def part2():
    result = 0
    with open("input2.txt") as f:
        for line in f:
            (policy, (char,_), passcode) = line.split()
            min_p, max_p = policy.split("-")
            if (passcode[int(min_p)-1] == char and passcode[int(max_p)-1] != char) or (passcode[int(min_p)-1] != char and passcode[int(max_p)-1] == char):
                result += 1
        print(result)

part1()
part2()
