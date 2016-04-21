# reflow

Intelligently reflow text to a given length.

`reflow` is like `fold(1)` on steroids:

 - Break lines on word boundaries by default (mimicking `fold -s`)
 - Doesn't reflow markdown code blocks
 - Intelligently reflow markdown/email quote blocks, maintaining quote levels
 - Optionally ignores email headers

## Usage

```
Usage: reflow [-w|--width WIDTH] [--ignore-headers] [PATH]
  Intelligently reflow text to a given length

Available options:
  -h,--help                Show this help text
  -w,--width WIDTH         Specify a line width to use instead of the default 80
                           columns
  --ignore-headers         Don't wrap email headers
  PATH                     Path to input file. If not provided, reflow will
                           attempt to read from STDIN
```

## Development

Use [stack] for development. On OS X this can be installed via Homebrew:

```
$ brew install haskell-stack
```

Then run `bin/setup` to install the dependencies. Once you've got that set up,
you can test the build via `stack build`.

You can also install development versions via `stack install`.

## Tests

```
$ bin/test
```

This will run the `reflow` command against `fixtures/input.txt` and
check the diff against `fixtures/expected.txt`. If they are different, the
diff will be printed and the command will fail.
