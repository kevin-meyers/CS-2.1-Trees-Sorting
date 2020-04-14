#!python

def counting_sort(numbers):
    """Sort given numbers (integers) by counting occurrences of each number,
    then looping over counts and copying that many numbers into output list.
    TODO: Running time: ??? Why and under what conditions?
    TODO: Memory usage: ??? Why and under what conditions?"""
    def counts_of(nums):
        ''' O(2n + range)  '''
        start = min(nums)  # O(n)
        end = max(nums)  # O(n)

        result = [0] * (end - start + 1)  # O(range)
        for num in nums:  # O(n)
            result[num - start] += 1  # O(1)

        return enumerate(result, start)  # O(1)

    def expand_from(counts):
        ''' O(range + n) '''
        result = []
        for num, count in counts:  # O(range)
            result.extend([num] * count)  # O(1) or O(n / range)

        return result


    if len(numbers) < 2:
        return numbers

    # O(3n + 2range) = O(n + range)
    numbers[:] = expand_from(counts_of(numbers))



    # FIXME: Improve this to mutate input instead of creating new output list


def bucket_sort(numbers, num_buckets=10):
    """Sort given numbers by distributing into buckets representing subranges,
    then sorting each bucket and concatenating all buckets in sorted order.

    O(n + range + nb) = O(nb + range) range is based on counting sort
    TODO: Running time: ??? Why and under what conditions?
    TODO: Memory usage: ??? Why and under what conditions?"""
    def build_compare_with(item):
        '''  I seperated this function in case I want to change sorting
        direction.
        '''
        return lambda x: x < item

    def expand_from(buckets):
        ''' O(b * 2n/b + b * step) =  O(n + range) '''
        result = []
        for bucket in buckets:  # O(b)
            counting_sort(bucket)  # O(n/b + step)
            result.extend(bucket) # O(n/b)

        return result


    start = min(numbers)  # O(n)
    end = max(numbers)  # O(n)

    # O(b * step) = O(range)
    step = (end - start) // num_buckets + 1  # O(1)

    # O(b) b is num_buckets
    ranges = [build_compare_with(bound) for bound in range(start+step, end+step, step)]
    buckets = [[] for _ in range(num_buckets)]  # O(b)
    for num in numbers:  # O(n)
        # Consider using binary search to find the bucket each num belongs in
        for index, larger_than in enumerate(ranges):  # O(b)
            if larger_than(num):  # O(1)
                buckets[index].append(num)  # O(1)
                break

    # bucket building is O(3n + 2b + nb) = O(n + b + nb) = O(n * b)
    numbers[:] = expand_from(buckets) # O(n + range)


    # FIXME: Improve this to mutate input instead of creating new output list
