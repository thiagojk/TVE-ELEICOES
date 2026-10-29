"""Download público via gdown; nunca versiona o arquivo de 1,58 GB."""
from pathlib import Path
import gdown
out = Path("data/raw/votacao_secao_2026_BR.csv")
out.parent.mkdir(parents=True, exist_ok=True)
if not out.exists():
    tmp = out.with_suffix(".part")
    result = gdown.download(id="1iGD6lRUYgIxg8dXFkNr1nFF72x2K-OlZ", output=str(tmp), quiet=False)
    if result is None:
        raise SystemExit("Drive não permitiu download: baixe manualmente e defina TVE_DATA.")
    with tmp.open("rb") as stream:
        head = stream.read(4096)
    if b"<html" in head.lower() or b"QT_VOTOS" not in head:
        raise SystemExit("Resposta não parece CSV eleitoral; arquivo temporário preservado para inspeção.")
    tmp.rename(out)
