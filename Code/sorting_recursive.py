#!python
from random import randint

from sorting_iterative import bubble_sort


def merge(items1, items2):
    """Merge given lists of items, each assumed to already be in sorted order,
    and return a new list containing all items in sorted order.
    TODO: Running time: ??? Why and under what conditions?
    TODO: Memory usage: ??? Why and under what conditions?"""
    index_1, index_2 = 0, 0

    result = []

    while index_1 < len(items1) and index_2 < len(items2):
        if items1[index_1] <= items2[index_2]:
            result.append(items1[index_1])
            index_1 += 1

        else:
            result.append(items2[index_2])
            index_2 += 1

    while index_1 < len(items1):
        result.append(items1[index_1])
        index_1 += 1

    while index_2 < len(items2):
        result.append(items2[index_2])
        index_2 += 1

    result.extend(items1[index_1:] + items2[index_2:])
    return result



def left_half_of(items):
    return items[:len(items)//2]

def right_half_of(items):
    return items[len(items)//2:]


def split_sort_merge(items):
    """Sort given items by splitting list into two approximately equal halves,
    sorting each with an iterative sorting algorithm, and merging results into
    a list in sorted order.
    TODO: Running time: ??? Why and under what conditions?
    TODO: Memory usage: ??? Why and under what conditions?"""
    first_half = items[len(items)//2:]
    second_half = items[:len(items)//2]

    bubble_sort(first_half)
    bubble_sort(second_half)

    items[:] = merge(first_half, second_half)


def _merge_sort(items):
    """Sort given items by splitting list into two approximately equal halves,
    sorting each recursively, and merging results into a list in sorted order.
    TODO: Running time: ??? Why and under what conditions?
    TODO: Memory usage: ??? Why and under what conditions?"""
    if len(items) <= 1:
        return items

    return merge(_merge_sort(left_half_of(items)), _merge_sort(right_half_of(items)))

def merge_sort(items):
    items[:] = _merge_sort(items)

def partition(items, low, high):
    """Return index `p` after in-place partitioning given items in range
    `[low...high]` by choosing a pivot (TODO: document your method here) from
    that range, moving pivot into index `p`, items less than pivot into range
    `[low...p-1]`, and items greater than pivot into range `[p+1...high]`.
    TODO: Running time: ??? Why and under what conditions?
    TODO: Memory usage: ??? Why and under what conditions?"""

    pivot_index = randint(low, high)
    items[pivot_index], items[high] = items[high], items[pivot_index]
    pivot_index = high
    pivot = items[pivot_index]

    left_offset = low
    i = low
    while i <= high:
        if items[i] < pivot:
            items[left_offset], items[i] = items[i], items[left_offset]
            left_offset += 1

        i += 1


    items[pivot_index], items[left_offset] = items[left_offset], items[pivot_index]

    return left_offset


if __name__ == '__main__':
    l = [1, 19, 4, 4, 4, 4, 4, 2, 25, 100, 13]
    pivot_index = partition(l, 0, len(l)-1)
    pivot = l[pivot_index]
    for index, item in enumerate(l):
        if index == pivot_index:
            assert item == pivot

        elif index < pivot_index:
            assert item < pivot

        else:
            assert item >= pivot



def quick_sort(items, low=None, high=None):
    """Sort given items in place by partitioning items in range `[low...high]`
    around a pivot item and recursively sorting each remaining sublist range.
    TODO: Best case running time: ??? Why and under what conditions?
    TODO: Worst case running time: ??? Why and under what conditions?
    TODO: Memory usage: ??? Why and under what conditions?"""
    # TODO: Check if high and low range bounds have default values (not given)
    # TODO: Check if list or range is so small it's already sorted (base case)
    # TODO: Partition items in-place around a pivot and get index of pivot
    # TODO: Sort each sublist range by recursively calling quick sort
