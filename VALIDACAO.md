# Validação da entrega — 29/10/2026

## Executado

- Download do RAR fornecido no segundo link e extração do CSV de 1.578.545.088 bytes.
- MD5 do CSV: `876db34e307f6db8820b34c017b4ff4f`.
- Inventário real de colunas, turnos, cargos, candidatos e ausências.
- Pipeline completo em R 4.5.0, com 199 réplicas bootstrap por grupo/modelo elegível.
- Após revisão da não regularidade, reexecução da sensibilidade para omitir ICs não justificáveis.
- Testes de integridade, duplicidades, seção incompleta, fórmula de retorno, estimação e invariância dos erros-padrão à mudança de unidade.
- Renderização das três páginas no Quarto 1.7.32.
- Verificação dos arquivos e âncoras dos links locais: zero links quebrados.
- Inspeção visual dos histogramas regionais e do gráfico de estabilidade de xi.
- Leitura da estrutura YAML do workflow, eventos e dependência build → deploy.

## Base observada

497.787 seções brasileiras completas; 1.308 seções completas do exterior separadas.
110 seções com apenas um dos candidatos registrado foram colocadas em quarentena.
153 seções nacionais com menos de 30 votos foram mantidas. A hipótese alternativa
“linha ausente = zero” é apresentada como cenário descritivo, sem imputação na análise principal.
Os rótulos pedidos foram mapeados aos nomes completos encontrados no CSV e aos códigos observados 13 e 22.
A origem oficial e autenticidade do arquivo não foram certificadas independentemente.

## Limitações estatísticas que permanecem nos resultados

Os dez ajustes GPD são exploratórios, selecionados por regra operacional que exige
revisão dos gráficos. Há estimativa positiva de xi e endpoints/retornos incompatíveis
com o suporte da variável em alguns ajustes. Isso está apresentado como limitação.
Blocos de tamanhos heterogêneos impediram GEV em quatro regiões; no Nordeste,
a GEV de Lula é não regular, e seus ICs de Wald e bootstrap foram omitidos.
Bootstrap de zonas condiciona os limiares e não elimina dependência entre zonas.
O relatório não estabelece diferenças causais ou testa igualdade de xi pela simples sobreposição de ICs.

## Reprodutibilidade e publicação

`renv.lock` fixa as versões e `renv/activate.R` fornece o bootstrap oficial do renv 1.1.5.
As análises e a renderização foram executadas com as dependências disponíveis neste ambiente.
A restauração integral em uma instalação R padrão e a execução remota do Actions devem
ser confirmadas pela CI; o ambiente local usa uma instalação R relocada e não permite
testar essa restauração como se fosse uma instalação padrão. Nenhum sucesso de CI é presumido.

O workflow está configurado, mas a ativação de Pages é uma configuração do GitHub:
selecione Settings → Pages → GitHub Actions. A ferramenta conectada disponível não
oferece alteração dessa configuração. Um site publicado só deve ser anunciado após
confirmar o deployment. Os HTMLs já renderizados acompanham o pacote de entrega.

As figuras são geradas por `scripts/run.R`. O pacote de entrega inclui os PNGs;
o repositório mantém o código e as tabelas para revisão, sem o CSV bruto.
