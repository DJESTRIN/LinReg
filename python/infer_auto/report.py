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


def _condense_eda_summary(eda_summary: Dict[str, Any]) -> List[Dict[str, Any]]:
    """Flatten the (fairly deep) EDA summary produced by eda.py into a single
    row per dependent variable, for compact display in the report instead of
    dumping the full nested dict."""
    rows: List[Dict[str, Any]] = []
    for name, summary in (eda_summary or {}).items():
        normality = summary.get("normality") or {}
        transform = summary.get("transform") or {}
        stats = summary.get("summary") or {}
        rows.append(
            {
                "variable": name,
                "distribution": summary.get("distribution"),
                "normal": normality.get("normal"),
                "normality_p": normality.get("p"),
                "skew": normality.get("skew"),
                "suggested_transform": transform.get("selected"),
                "n": stats.get("n"),
                "mean": stats.get("mean"),
                "variance": stats.get("variance"),
                "min": stats.get("min"),
                "max": stats.get("max"),
            }
        )
    return rows


MD_TEMPLATE = Template(
    """# infeR Analysis Report

## Modeling decisions
{% if decision %}
- **Data shape used:** {{ decision.shape_used }} ({{ decision.shape_rationale }})
- **Resolved model family:** {{ decision.resolved_model_family }}
- **Resolved distribution:** {{ decision.resolved_distribution }}
- **Resolved transformation:** {{ decision.resolved_transformation }}
- **Candidate families considered:** {{ decision.candidate_families | join(", ") }}
- **Decision rationale:**
{% for item in decision.rationale %}
  - {{ item }}
{% endfor %}
{% else %}
_No decision log available._
{% endif %}

## Exploratory data analysis
{% if eda_table %}
{{ eda_table }}
{% else %}
_No EDA summary available._
{% endif %}

## Model summary

- **Model family:** {{ results.model_family_used or results.model_family }}
- **Formula:** `{{ results.formula }}`
- **Backbone package:** {{ results.backbone_package or "N/A" }}
- **Convergence OK:** {{ results.convergence_ok }}
- **AIC / BIC / logLik:** {{ aic_display }} / {{ bic_display }} / {{ loglik_display }}
- **RÂ²:** {{ r_squared_display }}
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

## Term-drop AIC comparison

{{ term_comparison_table }}

## Significant terms (auto post-hoc trigger)

{{ significant_terms_table }}

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
  <title>infeR Analysis Report</title>
  <style>
    body { font-family: "Georgia", "Times New Roman", serif; margin: 0 auto; max-width: 980px; padding: 2rem; color: #1a1a1a; line-height: 1.5; }
    h1 { border-bottom: 3px solid #2C7FB8; padding-bottom: 0.4rem; }
    h2 { border-bottom: 1px solid #ccc; padding-bottom: 0.2rem; margin-top: 2rem; color: #1b4f72; }
    h3 { color: #2C7FB8; }
    table { border-collapse: collapse; width: 100%; margin-bottom: 1.5rem; font-family: "Helvetica Neue", Arial, sans-serif; font-size: 0.92rem; }
    th, td { border: 1px solid #ccc; padding: 0.5rem 0.7rem; text-align: left; }
    th { background: #f2f6fa; }
    tr:nth-child(even) { background: #fafbfc; }
    .plot-grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(360px, 1fr)); gap: 1rem; margin-bottom: 1.5rem; }
    .plot-grid figure { margin: 0; text-align: center; }
    .plot-grid figcaption { font-size: 0.85rem; color: #555; margin-top: 0.3rem; }
    img { max-width: 100%; border: 1px solid #ddd; border-radius: 4px; }
    .notice { padding: 0.75rem; margin-bottom: 1rem; border-radius: 4px; }
    .warning { background: #fff7d6; }
    .error { background: #ffe0e0; }
  </style>
</head>
<body>
  <h1>infeR Analysis Report</h1>

  <h2>Modeling decisions</h2>
  {% if decision %}
  <ul>
    <li><strong>Data shape used:</strong> {{ decision.shape_used }} ({{ decision.shape_rationale }})</li>
    <li><strong>Resolved model family:</strong> {{ decision.resolved_model_family }}</li>
    <li><strong>Resolved distribution:</strong> {{ decision.resolved_distribution }}</li>
    <li><strong>Resolved transformation:</strong> {{ decision.resolved_transformation }}</li>
    <li><strong>Candidate families considered:</strong> {{ decision.candidate_families | join(", ") }}</li>
  </ul>
  <p><strong>Decision rationale:</strong></p>
  <ul>
    {% for item in decision.rationale %}
    <li>{{ item }}</li>
    {% endfor %}
  </ul>
  {% else %}
  <p><em>No decision log available.</em></p>
  {% endif %}

  <h2>Exploratory data analysis</h2>
  {% if eda_rows %}
  <table>
    <thead>
      <tr>{% for key in eda_rows[0].keys() %}<th>{{ key }}</th>{% endfor %}</tr>
    </thead>
    <tbody>
      {% for row in eda_rows %}
      <tr>{% for value in row.values() %}<td>{{ value }}</td>{% endfor %}</tr>
      {% endfor %}
    </tbody>
  </table>
  {% else %}
  <p><em>No EDA summary available.</em></p>
  {% endif %}

  <h2>Model summary</h2>
  <ul>
    <li><strong>Model family:</strong> {{ results.model_family_used or results.model_family }}</li>
    <li><strong>Formula:</strong> <code>{{ results.formula }}</code></li>
    <li><strong>Backbone package:</strong> {{ results.backbone_package or "N/A" }}</li>
    <li><strong>Convergence OK:</strong> {{ results.convergence_ok }}</li>
    <li><strong>AIC / BIC / logLik:</strong> {{ aic_display }} / {{ bic_display }} / {{ loglik_display }}</li>
    <li><strong>RÂ²:</strong> {{ r_squared_display }}</li>
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
    <div class="plot-grid">
    {% for plot in plots %}
      <figure>
        <img src="{{ plot }}" alt="{{ plot }}" />
        <figcaption>{{ plot }}</figcaption>
      </figure>
    {% endfor %}
    </div>
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
        "Term-drop AIC comparison": _as_list_of_dicts(results.get("term_comparison") or []),
        "Significant terms (auto post-hoc trigger)": _as_list_of_dicts(results.get("significant_terms") or []),
        "Post-hoc results": _as_list_of_dicts(results.get("posthoc") or []),
        "Effect sizes": _as_list_of_dicts(results.get("effect_sizes") or []),
    }
    eda_rows = _condense_eda_summary(results.get("eda_summary") or {})

    markdown_text = MD_TEMPLATE.render(
        results=results,
        plots=plots,
        decision=results.get("decision"),
        eda_table=_markdown_table(eda_rows),
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
        term_comparison_table=_markdown_table(tables["Term-drop AIC comparison"]),
        significant_terms_table=_markdown_table(tables["Significant terms (auto post-hoc trigger)"]),
        posthoc_table=_markdown_table(tables["Post-hoc results"]),
        effect_sizes_table=_markdown_table(tables["Effect sizes"]),
    )

    html_text = HTML_TEMPLATE.render(
        results=results,
        plots=plots,
        tables=tables,
        decision=results.get("decision"),
        eda_rows=eda_rows,
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
