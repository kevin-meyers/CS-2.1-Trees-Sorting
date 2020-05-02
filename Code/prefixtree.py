#!python3

from prefixtreenode import PrefixTreeNode


class PrefixTree:
    """PrefixTree: A multi-way prefix tree that stores strings with efficient
    methods to insert a string into the tree, check if it contains a matching
    string, and retrieve all strings that start with a given prefix string.
    Time complexity of these methods depends only on the number of strings
    retrieved and their maximum length (size and height of subtree searched),
    but is independent of the number of strings stored in the prefix tree, as
    its height depends only on the length of the longest string stored in it.
    This makes a prefix tree effective for spell-checking and autocompletion.
    Each string is stored as a sequence of characters along a path from the
    tree's root node to a terminal node that marks the end of the string."""

    # Constant for the start character stored in the prefix tree's root node
    START_CHARACTER = ''

    def __init__(self, strings=None):
        """Initialize this prefix tree and insert the given strings, if any."""
        # Create a new root node with the start character
        self.root = PrefixTreeNode(PrefixTree.START_CHARACTER)
        # Count the number of strings inserted into the tree
        self.size = 0
        # Insert each string, if any were given
        if strings is not None:
            for string in strings:
                self.insert(string)

    class decorators:
        @staticmethod
        def preprocess(func):
            def inner(self, string, *args, **kwargs):
                processed = ''.join([x for x in string if x.isalpha()]).upper()
                return func(self, processed, *args, **kwargs)
            return inner

    def __repr__(self):
        """Return a string representation of this prefix tree."""
        return f'PrefixTree({self.strings()!r})'

    def is_empty(self):
        """Return True if this prefix tree is empty (contains no strings)."""
        return self.size == 0

    @decorators.preprocess
    def contains(self, string):
        """Return True if this prefix tree contains the given string."""
        current, depth_gone = self._find_node(string)

        return depth_gone == len(string) and current.is_terminal()

    @decorators.preprocess
    def insert(self, string):
        """Insert the given string into this prefix tree."""
        current, depth_gone = self._find_node(string)
        for character in string[depth_gone:]:
            current[character] = PrefixTreeNode(character)
            current = current[character]

        # if string already exists
        if current.terminal is not True:
            current.terminal = True
            self.size += 1

    def _find_node(self, string):
        """Return a pair containing the deepest node in this prefix tree that
        matches the longest prefix of the given string and the node's depth.
        The depth returned is equal to the number of prefix characters matched.
        Search is done iteratively with a loop starting from the root node."""
        current = self.root
        depth = 0

        # Start with the root node
        for character in string:
            if character not in current:
                break

            current = current[character]
            depth += 1

        return current, depth

    @decorators.preprocess
    def complete(self, prefix):
        """Return a list of all strings stored in this prefix tree that start
        with the given prefix string."""
        # Create a list of completions in prefix tree
        result = []
        node, depth_gone = self._find_node(prefix)
        if depth_gone == len(prefix):
            self._traverse(node, prefix, result.append)

        return result

    def strings(self):
        """Return a list of all strings stored in this prefix tree."""
        # Create a list of all strings in prefix tree
        result = []

        self._traverse(self.root, PrefixTree.START_CHARACTER, result.append)
        return result

    def _traverse(self, node, prefix, visit):
        """Traverse this prefix tree with recursive depth-first traversal.
        Start at the given node with the given prefix representing its path in
        this prefix tree and visit each node with the given visit function."""
        if node.is_terminal():
            visit(prefix)

        for child in node.children:
            self._traverse(child, prefix + child.character, visit)

    @decorators.preprocess
    def delete(self, string):
        def recursion(node, index):
            if index >= len(string):
                return node.is_terminal() and node.num_children() == 0

            next_char = string[index]

            if next_char not in node:
                return False

            did_delete = recursion(node[next_char], index + 1)

            if did_delete:
                node.remove_child(next_char)
                if not node.is_terminal() and node.num_children() == 0:
                    return True

            return False
        return recursion(self.root, 0)



def create_prefix_tree(strings):
    print(f'strings: {strings}')

    tree = PrefixTree()
    print(f'\ntree: {tree}')
    print(f'root: {tree.root}')
    print(f'strings: {tree.strings()}')

    print('\nInserting strings:')
    for string in strings:
        tree.insert(string)
        print(f'insert({string!r}), size: {tree.size}')

    print(f'\ntree: {tree}')
    print(f'root: {tree.root}')

    print('\nSearching for strings in tree:')
    for string in sorted(set(strings)):
        result = tree.contains(string)
        print(f'contains({string!r}): {result}')

    print('\nSearching for strings not in tree:')
    prefixes = sorted(set(string[:len(string)//2] for string in strings))
    for prefix in prefixes:
        if len(prefix) == 0 or prefix in strings:
            continue
        result = tree.contains(prefix)
        print(f'contains({prefix!r}): {result}')

    print('\nCompleting prefixes in tree:')
    for prefix in prefixes:
        completions = tree.complete(prefix)
        print(f'complete({prefix!r}): {completions}')

    print('\nRetrieving all strings:')
    retrieved_strings = tree.strings()
    print(f'strings: {retrieved_strings}')
    matches = set(retrieved_strings) == set(strings)
    print(f'matches? {matches}')


if __name__ == '__main__':
    # Create a dictionary of tongue-twisters with similar words to test with
    TONGUE_TWISTERS = {
        'Seashells': 'Shelly sells seashells by the sea shore'.split(),
        # 'Peppers': 'Peter Piper picked a peck of pickled peppers'.split(),
        # 'Woodchuck': ('How much wood would a wood chuck chuck'
        #                ' if a wood chuck could chuck wood').split()
    }
    # Create a prefix tree with the similar words in each tongue-twister
    for name, s in TONGUE_TWISTERS.items():
        print('\n' + '='*80 + '\n')
        print(f'{name} tongue-twister:')
        create_prefix_tree(s)
