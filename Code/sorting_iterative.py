#!python


def is_sorted(items):
    """Return a boolean indicating whether given items are in sorted order.
    TODO: Running time: O(n) because it loops once till it finds any out of place
    TODO: Memory usage: O(1) because we arent making any copies etc"""
    if len(items) < 2:
        return True

    previous_item = items[0]
    for current_item in items[1:]:
        if current_item < previous_item:
            return False

        previous_item = current_item

    return True


def swap_items(items, first_i, second_i):
    items[first_i], items[second_i] = items[second_i], items[first_i]

def bubble_sort(items, ascending=True, key=None):
    """Sort given items by swapping adjacent items that are out of order, and
    repeating until all items are in sorted order.
    TODO: Running time: O(n^2) triangular but...
    TODO: Memory usage: O(1) Why and under what conditions?"""
    if key is None:
        key = lambda x: x

    def compare(x, y):
        if ascending:
            return key(x) > key(y)

        return key(x) < key(y)

    something_swapped = True

    while something_swapped:
        something_swapped = False
        prev_i = 0

        while prev_i + 1 < len(items):
            if compare(items[prev_i], items[prev_i + 1]):
                swap_items(items, prev_i, prev_i + 1)
                something_swapped=True

            prev_i += 1


def selection_sort(items):
    """Sort given items by finding minimum item, swapping it with first
    unsorted item, and repeating until all items are in sorted order.
    TODO: Running time: O(n^2) O(n) swaps
    TODO: Memory usage: O(1) """
    for sorted_index in range(len(items)):
        min_index = len(items) - 1
        min_num = items[-1]

        for index, item in enumerate(items[sorted_index:], start=sorted_index):
            if item < min_num:
                min_num = item
                min_index = index

        swap_items(items, sorted_index, min_index)


def insertion_sort(items):
    """Sort given items by taking first unsorted item, inserting it in sorted
    order in front of items, and repeating until all items are in order.
    TODO: Running time: O(n^2) mine is O(n^3) Why and under what conditions?
    TODO: Memory usage: O(1) Why and under what conditions?"""
    sorted_index = 0
    while sorted_index < len(items):
        for index, s_item in enumerate(items[:sorted_index]):
            if s_item > items[sorted_index]:
                items.insert(index, items.pop(sorted_index))

        sorted_index += 1

    return items



def binary_find_index(items, x, start=None, end=None):
    if start is None:
        start = 0
        end = len(items) - 1

    if items == []:
        return end

    middle = (start + end) // 2

    if start >= end:
        return end if x <= items[middle] else end + 1

    if x > items[middle]:
        return binary_find_index(items, x, middle + 1, end)

    else:
        return binary_find_index(items, x, start, middle)


def binary_insertion_sort(items):
    sorted_index = 0

    while sorted_index < len(items):
        insert_index = binary_find_index(items[:sorted_index], items[sorted_index])
        items.insert(insert_index, items.pop(sorted_index))

        sorted_index += 1

    return items


def bubble_item(items, i):
    if items[i] == items[i + 1]:
        return False, True

    if items[i] > items[i + 1]:
        swap_items(items, i, i+1)
        return True, False

    return False, False


def bubble_pass(items, start, end):
    something_swapped = False
    swapped_same = False
    for i in range(start, end):
        swapped, same = bubble_item(items, i)
        something_swapped = something_swapped or swapped
        swapped_same = swapped_same or same

    return something_swapped, swapped_same


def cocktail_shaker_sort(items):
    swapped_same = False
    something_swapped = True
    offset = 0
    while something_swapped:

        something_swapped, same = bubble_pass(items, offset, len(items) - 2 - offset)
        swapped_same = swapped_same or same


        if not something_swapped:
            break

        something_swapped, same = bubble_pass(items, offset + 2, len(items) - offset)
        swapped_same = swapped_same or same

        if not swapped_same:
            offset += 1
