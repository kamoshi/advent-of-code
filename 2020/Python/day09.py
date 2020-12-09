input_data = []


with open("input.txt") as file:
    for line in file:
        input_data.append(int(line.rstrip()))


def solve_p1(data: list[int], preamble: int) -> int:

    def check_number(number: int, slice: list[int]):
        for i in range(len(slice)):
            for j in range(i + 1, len(slice)):
                if number == slice[i] + slice[j]:
                    return True
        return False

    for i in range(preamble, len(data)):
        if not check_number(data[i], data[i-preamble:i]):
            return data[i]

    return -1


def solve_p2(data: list[int], solution_p1: int) -> int:

    for i in range(len(data)):
        for j in range(i + 1, len(data)):
            slice = data[i:j]
            if solution_p1 == sum(slice):
                return min(slice) + max(slice)
    return -1


print(solution1 := solve_p1(input_data, 25))
print(solve_p2(input_data, solution1))
