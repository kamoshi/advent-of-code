class LanternFish:
    def __init__(self, cycle: int, current: int = None):
        self.cycle = cycle
        self.current = current or cycle
        self.children = []

    def advance(self) -> None:
        self.current -= 1
        [child.advance() for child in self.children]
        if self.current == -1:
            self.current = self.cycle
            self.children.append(LanternFish(cycle=6, current=8))
    
    def count(self) -> int:
        return 1 + sum([child.count() for child in self.children])


def load() -> list[int]:
    with open('../.input/day06') as f:
        return [int(line) for line in f.read().split(",")]


def solve1() -> int:
    fishes = [LanternFish(cycle=6, current=init) for init in load()]
    [[fish.advance() for fish in fishes] for _ in range(80)]
    return sum([fish.count() for fish in fishes])


def solve2() -> int:
    fishes = { i: 0 for i in range(7) }
    newborns = { i: 0 for i in range(9) }

    for init in load():
        fishes[init] += 1
    
    for epoch in range(256):
        born_from_old = fishes[epoch % 7]
        fishes[epoch % 7] += newborns[epoch % 9]
        newborns[epoch % 9] += born_from_old
    
    return sum(fishes.values()) + sum(newborns.values())



if __name__ == "__main__":
    print(solve1())  # 379414
    print(solve2())  # 1705008653296
