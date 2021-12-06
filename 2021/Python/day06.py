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
    [[fish.advance() for fish in fishes] for _ in range(100)]
    return sum([fish.count() for fish in fishes])


if __name__ == "__main__":
    print(solve1())