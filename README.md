# TVE — concentrações de votos por seção eleitoral

Projeto em R/Quarto para validar dados, descrever proporções e investigar POT/GPD e
máximos por zona/GEV. Não presume adequação dos modelos ou diferenças entre regiões.
O PDF de OPME foi usado como referência estrutural, sem copiar texto ou modelos de custos.

## Dados e proveniência

Fonte fornecida pelo usuário: [CSV (1,58 GB)](https://drive.google.com/file/d/1iGD6lRUYgIxg8dXFkNr1nFF72x2K-OlZ/view).
Alternativa compactada: [RAR](https://drive.google.com/file/d/1hhEN4KziAUc6DbEhU3YQTbTI2l15-_OQ/view).
Nomes observados: LUIZ INÁCIO LULA DA SILVA (13) e FLAVIO NANTES BOLSONARO (22), mapeados aos rótulos solicitados em `config.R`.
O nome do arquivo não autentica origem oficial. O pipeline verifica ano, turno, cargo e
nomes dos candidatos. Os dados brutos nunca entram no Git. Não publique o arquivo de 1,58 GB.

## Reprodução

Requer R 4.5, Quarto 1.7 ou superior e memória livre suficiente para o CSV (recomenda-se 8 GB).
Execute na raiz do repositório:

```r
install.packages("renv", repos = "https://cloud.r-project.org")
renv::restore(prompt = FALSE)
```

Baixe e extraia o CSV em `data/raw/votacao_secao_2026_BR.csv`, ou use o download público:

```sh
python -m pip install gdown==5.2.0
python scripts/download_data.py
Rscript scripts/run.R
quarto render
quarto preview
```

O download automático depende das permissões e cotas do Drive. Se falhar, use download
manual; não substitua por dados simulados. Para outro caminho/encoding:

```sh
TVE_DATA=data/raw/outro.csv TVE_ENCODING=Latin-1 Rscript scripts/run.R
```

Para atualizar a base, preserve a original, configure o novo caminho, revise `config.R`,
execute a análise e renderize novamente. O hash e `sessionInfo` registram a execução.
O separador esperado é ponto e vírgula, explicitamente configurável em `config.R`.

## Organização e revisão

- `R/01_preparacao.R`: auditorias, seções incompletas e base analítica.
- `R/02_descritiva.R`: quantis, gráficos, blocos e agrupamentos.
- `R/03_gpd.R`: grade de thresholds e ajustes por região/candidato.
- `R/04_gev.R`: elegibilidade dos blocos e GEV.
- `R/05_diagnosticos.R`: QQ, PP, CDF, densidade e retorno.
- `R/06_sensibilidade.R`: thresholds vizinhos, tamanho e bootstrap de zonas.
- `tables/`: resultados desta execução; `figures/`: PNGs gerados pela análise (também incluídos no pacote de entrega); `data/processed/`: objetos locais ignorados pelo Git.
- `relatorio.qmd`, `apendices.qmd`: texto acadêmico e diagnósticos.

Leia `config.R` e os critérios metodológicos do relatório. A escolha automática de threshold
é exploratória e deve ser revisada com os gráficos. Massa em 1, suporte incompatível,
xi <= -0,5 e falhas ficam explícitos. ICs iid não corrigem dependência. ICs bootstrap
condicionam o limiar e pressupõem zonas aproximadamente independentes. Não há teste causal.
Retornos POT contam seções; retornos GEV contam zonas, nunca anos.

O marcador `data/processed/CONCLUIDO` só é escrito ao fim de execução bem-sucedida.
Sem ele, o relatório renderiza um estado de análise pendente, sem resultados antigos.

## Verificação

```sh
Rscript tests/run.R
Rscript scripts/run.R
quarto render
```

Os testes usam dados sintéticos exclusivamente para verificar integridade e fórmulas;
esses dados não alimentam o relatório. Consulte `VALIDACAO.md` para o estado real
verificado nesta entrega. Não trate código existente como evidência de análise executada.

## GitHub Pages

Em **Settings → Pages → Build and deployment**, selecione **GitHub Actions**.
O workflow `.github/workflows/pages.yml` restaura dependências, baixa a base, executa
análises e testes, renderiza e publica `docs/` após push em `main` ou execução manual.
A publicação falha se o download/análise falhar, para não publicar resultados incompletos.
O ambiente `github-pages` pode exigir aprovação conforme as configurações do repositório.
O endereço será mostrado pela etapa de deployment após sucesso; não se presume site ativo.

O lockfile registra as versões e dependências disponíveis na construção inicial; a restauração
em outra plataforma deve ser confirmada pela CI. Para atualizar deliberadamente as dependências,
execute `renv::snapshot()` e revise o diff do lockfile.
