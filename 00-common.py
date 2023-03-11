"""
Some common things for use in iPython
"""
from subprocess import run, CompletedProcess


def print_bytes(b: bytes, encoding: str = "utf-8") -> None:
    if not isinstance(b, bytes):
        raise ValueError()

    print(str(b, encoding=encoding))


def run_cmd(command: list) -> (str, str):
    if not isinstance(command, list):
        raise ValueError()

    output: CompletedProcess = run(command, capture_output=True)
    return (output.stdout, output.stderr)
