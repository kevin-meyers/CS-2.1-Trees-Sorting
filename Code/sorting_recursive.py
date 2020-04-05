#!python

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

    return result



def split_sort_merge(items):
    """Sort given items by splitting list into two approximately equal halves,
    sorting each with an iterative sorting algorithm, and merging results into
    a list in sorted order.
    TODO: Running time: ??? Why and under what conditions?
    TODO: Memory usage: ??? Why and under what conditions?"""
    first_half = items[len(items)//2:]
    second_half = items[:len(items)//2]
    del items

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

    return merge(_merge_sort(items[:len(items)//2]), _merge_sort(items[len(items)//2:]))

def merge_sort(items):
    items[:] = _merge_sort(items)

def partition(items, low, high):
    """Return index `p` after in-place partitioning given items in range
    `[low...high]` by choosing a pivot (TODO: document your method here) from
    that range, moving pivot into index `p`, items less than pivot into range
    `[low...p-1]`, and items greater than pivot into range `[p+1...high]`.
    TODO: Running time: ??? Why and under what conditions?
    TODO: Memory usage: ??? Why and under what conditions?"""
    # TODO: Choose a pivot any way and document your method in docstring above
    # TODO: Loop through all items in range [low...high]
    # TODO: Move items less than pivot into front of range [low...p-1]
    # TODO: Move items greater than pivot into back of range [p+1...high]
    # TODO: Move pivot item into final position [p] and return index p


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
