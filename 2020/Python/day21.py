from typing import Tuple


def parse() -> Tuple[set[str], set[str], list[Tuple[list[str], list[str]]]]:
    ingredient_set = set()
    allergen_set = set()
    output = []
    with open("input.txt") as file:
        for line in file:
            ingredients, allergens = line.rstrip().split(" (contains ")
            ingredients_list = list(map(str.strip, ingredients.split(" ")))
            allergens_list = list(map(str.strip, allergens[:-1].split(",")))
            [ingredient_set.add(ingredient) for ingredient in ingredients_list]
            [allergen_set.add(allergen) for allergen in allergens_list]
            output.append((ingredients_list, allergens_list))
    return ingredient_set, allergen_set, output


def predict_ingredients(data: list[Tuple[list[str], list[str]]], allergen: str) -> list[str]:
    counted_occurrences = {}
    for ingredients, allergens in data:
        if allergen in allergens:
            for ingredient in ingredients:
                if ingredient not in counted_occurrences:
                    counted_occurrences[ingredient] = 0
                counted_occurrences[ingredient] += 1

    found_ingredients = set()
    for ingredients, allergens in data:
        if allergen in allergens:
            max_occurrences = max([val if key in ingredients else 0 for (key, val) in counted_occurrences.items()])
            for ingredient in ingredients:
                if counted_occurrences[ingredient] == max_occurrences:
                    found_ingredients.add(ingredient)

    return list(found_ingredients)


def solve_p1(data: list[Tuple[list[str], list[str]]], ingredients: set[str], allergens: set[str]) -> int:
    probable_ingredients = set()
    for allergen in allergens:
        for found in predict_ingredients(data, allergen):
            probable_ingredients.add(found)

    safe = ingredients.difference(probable_ingredients)
    counted = 0
    for ingredients, _ in data:
        for ingredient in ingredients:
            if ingredient in safe:
                counted += 1
    return counted


INGREDIENTS, ALLERGENS, DATA = parse()
print(solve_p1(DATA, INGREDIENTS, ALLERGENS))


def solve_p2(data: list[Tuple[list[str], list[str]]], ingredients: set[str], allergens: set[str]) -> str:
    assigned_allergens = sorted([(allergen, set(predict_ingredients(data, allergen))) for allergen in allergens], key=lambda e: len(e[1]))

    bijection = []
    while len(assigned_allergens[len(assigned_allergens)-1][1]) > 1:
        sorted_allergens = assigned_allergens.copy()
        first_allergen, first_set = sorted_allergens[0]
        bijection.append((first_allergen, first_set))
        assigned_allergens = []
        for i in range(1, len(sorted_allergens)):
            next_allergen, next_set = sorted_allergens[i]
            assigned_allergens.append((next_allergen, next_set.difference(first_set)))
        assigned_allergens = sorted(assigned_allergens, key=lambda e: len(e[1]))
    bijection += assigned_allergens

    result = ",".join([i for (a, i) in sorted(list(map(lambda elem: (elem[0], next(iter(elem[1]))), bijection)), key=lambda e: e[0])])
    return result


print(solve_p2(DATA, INGREDIENTS, ALLERGENS))
