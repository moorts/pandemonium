# pandemonium

Small tool to help convert chaotic output into json data for evaluation. I don't really know what exactly I want, but right now it works like this:

Let's say your program outputs data like so:

```
SOME_DELIMITER
time: 12
output: 1 5 2 9
path: ./path/to/some/file
SOME_DELIMITER (Start of next block)
```

Or like so:
```
SOME_DELIMITER
time: 5
random garbage

output: 2 5
random print statement: 1 5 2 3 2 4
path: ./path/to/other/file
SOME_DELIMITER (Start of next block)
...
```

Now the goal is to easily parse this into JSON without having to write or copy-paste together any python code. By specifying a config file (which I should probably call schema, but w/e) like this:

```json
{
    "sep": "SOME_DELIMITER",
    "stringKeys": ["path"],
    "floatKeys": [],
    "intKeys": [time, output],
}
```

You can do this by calling `pandemonium ./config.json ./path/to/data`.

I'd like to clarify again that this is not great and a work-in-progress. I feel like this could be practical, but right now it probably isn't.

Oh and this is in Haskell, because I wrote it in Haskell.
