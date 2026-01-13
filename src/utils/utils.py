from pathlib import Path


def get_files_in(path, extension):
    files = sorted(Path(path).glob(f"*.{extension}"))
    files = [str(p) for p in files]
    return files