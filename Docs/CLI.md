# Command line interface

## Options

### -trace-inference

Enable tracing of type inference requests at the given line.

**Example:**

```bash
valc --typecheck --trace-inference main.val:16 main.val
```

Running this command will show a trace of the type constraint solver for all root expressions at line 16 of `main.val`.