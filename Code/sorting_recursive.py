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
    """ Query helper for readability """
    return items[:len(items)//2]

def right_half_of(items):
    """ Query helper for readability """
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
    """ Helper for inplace. """
    items[:] = _merge_sort(items)


def make_swapper_of(items):
    """ Makes a swapping function, holds items in closure. """
    def swap(i_1, i_2):
        items[i_1], items[i_2] = items[i_2], items[i_1]

    return swap

def make_partitioner_of(items):
    """ Makes a partitioning function, holds items in closure. """
    def partitioner(low, high):
        """Return index `p` after in-place partitioning given items in range
        `[low...high]` by choosing a pivot (TODO: document your method here) from
        that range, moving pivot into index `p`, items less than pivot into range
        `[low...p-1]`, and items greater than pivot into range `[p+1...high]`.
        TODO: Running time: ??? Why and under what conditions?
        TODO: Memory usage: ??? Why and under what conditions?"""
        swap = make_swapper_of(items)

        # Select random pivot and move to the end
        pivot_index = randint(low, high)
        swap(pivot_index, high)

        # Grab pivot from end
        pivot_index_low = high
        pivot_index_high = high
        pivot = items[pivot_index_high]

        left_offset = low
        while low < pivot_index_low:
            if items[low] == pivot:
                pivot_index_low -= 1
                swap(low, pivot_index_low)
                low -= 1

            elif items[low] < pivot:
                swap(left_offset, low)
                left_offset += 1

            low += 1

        for i in range(pivot_index_high - pivot_index_low + 1):
            if left_offset + i >= pivot_index_low:
                break

            swap(left_offset + i, pivot_index_high - i)

        return [left_offset, left_offset + i]

        # swap(pivot_index_high, left_offset)
        return left_offset

    return partitioner


def quick_sort(items):
    """Sort given items in place by partitioning items in range `[low...high]`
    around a pivot item and recursively sorting each remaining sublist range.
    TODO: Best case running time: ??? Why and under what conditions?
    TODO: Worst case running time: ??? Why and under what conditions?
    TODO: Memory usage: ??? Why and under what conditions?"""
    partitioner = make_partitioner_of(items)

    def recursive_function(low, high):
        if high <= low:
            return

        pivot_low, pivot_high = partitioner(low, high)

        recursive_function(pivot_high + 1, high)
        recursive_function(low, pivot_low - 1)

    recursive_function(0, len(items)-1)


'''
if __name__ == '__main__':
    L = [1, 19, 4, 4, 4, 4, 4, 2, 25, 100, 13]
    partition = make_partitioner_of(L)
    PIVOT_INDEX = partition(0, len(L)-1)
    PIVOT = L[PIVOT_INDEX]
    for index, item in enumerate(L):
        if index == PIVOT_INDEX:
            assert item == PIVOT

        elif index < PIVOT_INDEX:
            assert item < PIVOT

        else:
            assert item >= PIVOT

'''
