import cmd
import readline


class LispyShell(cmd.Cmd):
    intro = 'PyLispy Version 0.0.0.0.1\nPress Ctrl+d to Exit'
    prompt = 'lispy> '

    def do_EOF(self, line):
        print()
        return True

    def default(self, line):
        print(line)


def main():
    shell = LispyShell(completekey=None)
    shell.cmdloop()


if __name__ == '__main__':
    main()
