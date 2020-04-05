#!python

def counting_sort(numbers):
    """Sort given numbers (integers) by counting occurrences of each number,
    then looping over counts and copying that many numbers into output list.
    TODO: Running time: ??? Why and under what conditions?
    TODO: Memory usage: ??? Why and under what conditions?"""
    def counts_of(n):
        start = min(n)
        end = max(n)

        result = [0] * (end - start + 1)
        for num in n:
            result[num - start] += 1

        return enumerate(result, start)

    def expand_from(counts):
        result = []
        for num, count in counts:
            result.extend([num] * count)

        return result


        if numbers == []:
            return []


    numbers[:] = expand_from(counts_of(numbers))



    # FIXME: Improve this to mutate input instead of creating new output list


def bucket_sort(numbers, num_buckets=10):
    """Sort given numbers by distributing into buckets representing subranges,
    then sorting each bucket and concatenating all buckets in sorted order.
    TODO: Running time: ??? Why and under what conditions?
    TODO: Memory usage: ??? Why and under what conditions?"""
    def build_compare_with(item):
        return lambda x: x < item

    def expand_from(b):
        result = []
        for bucket in buckets:
            counting_sort(bucket)
            result.extend(bucket)

        return result


    start = min(numbers)
    end = max(numbers)
    step = (end - start) // num_buckets + 1


    ranges = [build_compare_with(bound) for bound in range(start+step, end+step, step)]
    buckets = [[] for _ in range(num_buckets)]
    for num in numbers:
        for index, larger_than in enumerate(ranges):
            if larger_than(num):
                buckets[index].append(num)
                break

    numbers[:] = expand_from(buckets)


    # FIXME: Improve this to mutate input instead of creating new output list
