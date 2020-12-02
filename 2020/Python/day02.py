def part1():
    result = 0
    with open("input2.txt") as f:
        for line in f:
            (policy, char, passcode) = line.split()
            min_p, max_p = policy.split("-")
            number_of_char = passcode.count(char[0])
            if int(min_p) <= number_of_char <= int(max_p):
                result += 1
        
        print(result)


def part2():
    result = 0
    with open("input2.txt") as f:
        for line in f:
            (policy, char, passcode) = line.split()
            min_p, max_p = policy.split("-")
            if (passcode[int(min_p)-1] == char[0] and passcode[int(max_p)-1] != char[0]) or (passcode[int(min_p)-1] != char[0] and passcode[int(max_p)-1] == char[0]):
                result += 1
        print(result)

part1()
part2()
