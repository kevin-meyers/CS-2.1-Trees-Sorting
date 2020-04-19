#!python3


ALPHABET_LENGTH = 26
ASCII_LOWERCASE_START = 97
ASCII_UPPERCASE_START = 65

START_OFFSET = ASCII_UPPERCASE_START


class LetterList:
    """ LetterList is a class that abstracts away the instantiation and
    underlying datatype for PrefixTreeNode.
    """
    def __init__(self):
        self.letters = [None] * ALPHABET_LENGTH
        self.length = 0

    def __iter__(self):
        # TODO: consider keeping track of letters in the list that are filled.
        # also these are children NODES!
        for letter in self.letters:
            if letter is not None:
                yield letter

    @staticmethod
    def index_for(character):
        """ Helper function to get the index of a character. """
        return ord(character) - START_OFFSET

    def __len__(self):
        return self.length

    def __setitem__(self, character, child):
        self.letters[self.index_for(character)] = child
        self.length += 1

    def __getitem__(self, character):
        return self.letters[self.index_for(character)]

    def __contains__(self, character):
        return self[character] is not None

    def __eq__(self, other):
        return self.letters == other.letters

    def __repr__(self):
        """Return a code representation of this LetterList."""
        return f'LetterList({self.letters!r})'

    def __str__(self):
        """Return a string view of this LetterList."""
        return f'({self.letters})'


class PrefixTreeNode:
    """PrefixTreeNode: A node for use in a prefix tree that stores a single
    character from a string and a structure of children nodes below it, which
    associates the next character in a string to the next node along its path
    from the tree's root node to a terminal node that marks the end of the
    string.
    """

    # Choose a type of data structure to store children nodes in
    # Hint: Choosing list or dict affects implementation of all child methods
    CHILDREN_TYPE = LetterList

    def __init__(self, character=None):
        """Initialize this prefix tree node with the given character value, an
        empty structure of children nodes, and a boolean terminal property."""
        # Character that this node represents
        self.character = character
        # Data structure to associate character keys to children node values
        self.children = PrefixTreeNode.CHILDREN_TYPE()
        # Marks if this node terminates a string in the prefix tree
        self.terminal = False

    def is_terminal(self):
        """Return True if this prefix tree node terminates a string."""
        return self.terminal

    def num_children(self):
        """Return the number of children nodes this prefix tree node has."""
        return len(self.children)

    def __len__(self):
        return self.num_children()

    def has_child(self, character):
        """Return True if this prefix tree node has a child node that
        represents the given character amongst its children."""
        return character in self.children

    def __contains__(self, character):
        return self.has_child(character)

    def get_child(self, character):
        """Return this prefix tree node's child node that represents the given
        character if it is amongst its children, or raise ValueError if not."""
        if self.has_child(character):
            return self.children[character]

        raise ValueError(f'No child exists for character {character!r}')

    def __getitem__(self, character):
        return self.get_child(character)

    def add_child(self, character, child_node):
        """Add the given character and child node as a child of this node, or
        raise ValueError if given character is amongst this node's children."""
        if not self.has_child(character):
            self.children[character] = child_node

        else:
            raise ValueError(f'Child exists for character {character!r}')

    def __setitem__(self, character, child_node):
        self.add_child(character, child_node)

    def __repr__(self):
        """Return a code representation of this prefix tree node."""
        return f'PrefixTreeNode({self.character!r})'

    def __str__(self):
        """Return a string view of this prefix tree node."""
        return f'({self.character})'
