"""smoke.py INTRICACY_BINARY: start the game in a pseudo-terminal from
another directory with an empty HOME, open the first tutorial lock, and fail
unless it was found and drawn.

The release archive puts tutorial/ and initiation/ next to the binary; this
proves the binary finds them there rather than in a cabal data path."""
import fcntl, os, pty, select, signal, struct, sys, tempfile, termios, time

binary = os.path.abspath(sys.argv[1])
home = tempfile.mkdtemp()
os.chdir(tempfile.mkdtemp())
pid, fd = pty.fork()
if pid == 0:
    os.environ.update(TERM="xterm-256color", HOME=home)
    os.environ.pop("XDG_STATE_HOME", None)
    os.execv(binary, [binary])
# a new pty is 0x0, and the game draws nothing into that
fcntl.ioctl(fd, termios.TIOCSWINSZ, struct.pack("HHHH", 30, 100, 0, 0))

def read_for(seconds):
    out, end = b"", time.time() + seconds
    while time.time() < end:
        r, _, _ = select.select([fd], [], [], 0.2)
        if r:
            try:
                out += os.read(fd, 65536)
            except OSError:
                break
    return out

out = read_for(3)
os.write(fd, b"S")
out += read_for(3)
os.kill(pid, signal.SIGKILL)
text = out.decode("utf-8", "replace")
print(f"{len(out)} bytes of output")
if "Tutorial 1" not in text:
    print(text[-2000:])
    sys.exit("smoke test failed: the first tutorial lock did not open")
print("ok: the game found its tutorial beside the binary")
