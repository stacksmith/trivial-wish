# CL-WISH

A wish is a request for a later computation.

Note that this is not threaded, or related to promises and other multiprocessing libraries.

# Why?

There are situations that require delaying computation until later, while more data is gathered.  Rather than keeping track of the data yourself, you can create a wish class containing the data and 'fulfilling' the wish later.

This pattern repeats often enough to justify this miniscule library.

# Usage

Subclass your data from `wish:wish` and add your own slots.

Create a `fulfil` method for your wish.

Embed a `wishes` instance somewhere, to keep all the wishes.

Make wishrd by `(wish:make ...)` with an instance of your wish.  Continue with your code, possibly setting data in your wish or making other wishes.

Call `(fulfil wishes)` to finally invoke `fulfil` method on all the wishes




