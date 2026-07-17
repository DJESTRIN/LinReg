from __future__ import annotations

from pathlib import Path
from typing import Any, Dict, Iterable, List, Union

import markdown
from jinja2 import Template


def _as_list_of_dicts(rows: Iterable[Dict[str, Any]]) -> List[Dict[str, Any]]:
    return [dict(row) for row in rows or []]


def _markdown_table(rows: List[Dict[str, Any]]) -> str:
    if not rows:
        return "_None_"
    headers = list(rows[0].keys())
    lines = [
        "| " + " | ".join(headers) + " |",
        "| " + " | ".join(["---"] * len(headers)) + " |",
    ]
    for row in rows:
        lines.append("| " + " | ".join(str(row.get(header, "")) for header in headers) + " |")
    return "\n".join(lines)


def _display_value(value: Any) -> str:
    if value is None or value == "":
        return "N/A"
    return str(value)


MD_TEMPLATE = Template(
    """# LinReg Analysis Report

## Model summary

- **Model family:** {{ results.model_family_used or results.model_family }}
- **Formula:** `{{ results.formula }}`
- **Backbone package:** {{ results.backbone_package or "N/A" }}
- **Convergence OK:** {{ results.convergence_ok }}
- **AIC / BIC / logLik:** {{ aic_display }} / {{ bic_display }} / {{ loglik_display }}
- **R²:** {{ r_squared_display }}
- **Distribution parameters:** {{ distribution_parameters_display }}

{% if nonparametric_test %}
## Nonparametric test summary

{{ nonparametric_test_table }}
{% endif %}

## Residual normality

{{ results.residual_normality }}

## ANOVA table

{{ anova_table }}

## Coefficients

{{ coefficients_table }}

## Model comparison

{{ comparison_table }}

## Post-hoc results

{{ posthoc_table }}

## Effect sizes

{{ effect_sizes_table }}

## Diagnostic plots
{% if plots %}
{% for plot in plots %}
![]({{ plot }})
{% endfor %}
{% else %}
_No diagnostic plots provided._
{% endif %}

## Warnings
{% if results.warnings %}
{% for warning in results.warnings %}
- {{ warning }}
{% endfor %}
{% else %}
_None_
{% endif %}

## Errors
{% if results.errors %}
{% for error in results.errors %}
- {{ error }}
{% endfor %}
{% else %}
_None_
{% endif %}
"""
)

HTML_TEMPLATE = Template(
    """<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="utf-8" />
  <title>LinReg Analysis Report</title>
  <style>
    body { font-family: Arial, sans-serif; margin: 2rem; }
    table { border-collapse: collapse; width: 100%; margin-bottom: 1.5rem; }
    th, td { border: 1px solid #ccc; padding: 0.5rem; text-align: left; }
    img { max-width: 100%; margin-bottom: 1rem; }
    .notice { padding: 0.75rem; margin-bottom: 1rem; border-radius: 4px; }
    .warning { background: #fff7d6; }
    .error { background: #ffe0e0; }
  </style>
</head>
<body>
  <h1>LinReg Analysis Report</h1>
  <h2>Model summary</h2>
  <ul>
    <li><strong>Model family:</strong> {{ results.model_family_used or results.model_family }}</li>
    <li><strong>Formula:</strong> <code>{{ results.formula }}</code></li>
    <li><strong>Backbone package:</strong> {{ results.backbone_package or "N/A" }}</li>
    <li><strong>Convergence OK:</strong> {{ results.convergence_ok }}</li>
    <li><strong>AIC / BIC / logLik:</strong> {{ aic_display }} / {{ bic_display }} / {{ loglik_display }}</li>
    <li><strong>R²:</strong> {{ r_squared_display }}</li>
    <li><strong>Distribution parameters:</strong> {{ distribution_parameters_display }}</li>
  </ul>

  {% if nonparametric_test %}
  <h2>Nonparametric test summary</h2>
  <table>
    <thead>
      <tr>{% for key in nonparametric_test.keys() %}<th>{{ key }}</th>{% endfor %}</tr>
    </thead>
    <tbody>
      <tr>{% for value in nonparametric_test.values() %}<td>{{ value }}</td>{% endfor %}</tr>
    </tbody>
  </table>
  {% endif %}

  <h2>Residual normality</h2>
  <pre>{{ results.residual_normality }}</pre>

  {% for title, rows in tables.items() %}
  <h2>{{ title }}</h2>
  {% if rows %}
  <table>
    <thead>
      <tr>{% for key in rows[0].keys() %}<th>{{ key }}</th>{% endfor %}</tr>
    </thead>
    <tbody>
      {% for row in rows %}
      <tr>{% for value in row.values() %}<td>{{ value }}</td>{% endfor %}</tr>
      {% endfor %}
    </tbody>
  </table>
  {% else %}
  <p><em>None</em></p>
  {% endif %}
  {% endfor %}

  <h2>Diagnostic plots</h2>
  {% if plots %}
    {% for plot in plots %}
    <div><img src="{{ plot }}" alt="{{ plot }}" /></div>
    {% endfor %}
  {% else %}
    <p><em>No diagnostic plots provided.</em></p>
  {% endif %}

  {% if results.warnings %}
  <h2>Warnings</h2>
  {% for warning in results.warnings %}
  <div class="notice warning">{{ warning }}</div>
  {% endfor %}
  {% endif %}

  {% if results.errors %}
  <h2>Errors</h2>
  {% for error in results.errors %}
  <div class="notice error">{{ error }}</div>
  {% endfor %}
  {% endif %}
</body>
</html>
"""
)


def render_reports(results: Dict[str, Any], output_dir: Union[str, Path], report_format: str = "both") -> Dict[str, str]:
    output_path = Path(output_dir)
    output_path.mkdir(parents=True, exist_ok=True)
    plots = results.get("diagnostic_plots") or []
    nonparametric_test = dict(results.get("nonparametric_test") or {})
    tables = {
        "ANOVA table": _as_list_of_dicts(results.get("anova_table") or []),
        "Coefficients": _as_list_of_dicts(results.get("coefficients") or []),
        "Model comparison": _as_list_of_dicts(results.get("model_comparison") or []),
        "Post-hoc results": _as_list_of_dicts(results.get("posthoc") or []),
        "Effect sizes": _as_list_of_dicts(results.get("effect_sizes") or []),
    }

    markdown_text = MD_TEMPLATE.render(
        results=results,
        plots=plots,
        aic_display=_display_value(results.get("aic")),
        bic_display=_display_value(results.get("bic")),
        loglik_display=_display_value(results.get("logLik")),
        r_squared_display=_display_value(results.get("r_squared") or None),
        distribution_parameters_display=_display_value(results.get("distribution_parameters") or None),
        nonparametric_test=nonparametric_test,
        nonparametric_test_table=_markdown_table([nonparametric_test]) if nonparametric_test else "_None_",
        anova_table=_markdown_table(tables["ANOVA table"]),
        coefficients_table=_markdown_table(tables["Coefficients"]),
        comparison_table=_markdown_table(tables["Model comparison"]),
        posthoc_table=_markdown_table(tables["Post-hoc results"]),
        effect_sizes_table=_markdown_table(tables["Effect sizes"]),
    )

    html_text = HTML_TEMPLATE.render(
        results=results,
        plots=plots,
        tables=tables,
        aic_display=_display_value(results.get("aic")),
        bic_display=_display_value(results.get("bic")),
        loglik_display=_display_value(results.get("logLik")),
        r_squared_display=_display_value(results.get("r_squared") or None),
        distribution_parameters_display=_display_value(results.get("distribution_parameters") or None),
        nonparametric_test=nonparametric_test,
    )
    outputs: Dict[str, str] = {}

    if report_format in {"md", "both"}:
        md_path = output_path / "report.md"
        md_path.write_text(markdown_text, encoding="utf-8")
        outputs["md"] = str(md_path)

    if report_format in {"html", "both"}:
        html_path = output_path / "report.html"
        if report_format == "html":
            html_body = markdown.markdown(markdown_text, extensions=["tables"])
            html_text = f"<!DOCTYPE html><html><body>{html_body}</body></html>"
        html_path.write_text(html_text, encoding="utf-8")
        outputs["html"] = str(html_path)

    return outputs
