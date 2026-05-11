# EFA-Boosting Studio

## Overview

EFA-Boosting Studio is an interactive Shiny web application that
provides a user-friendly interface for running the EFA-Boosting
algorithm. It features real-time console output, interactive results
visualization, and export capabilities.

## Launching the Application

``` r

library(OptimalFactor)
run_efa_boosting()
```

The application will open in your default web browser.

## Interface Components

### Data Panel

- **File Upload**: Supports CSV and Excel (.xlsx) files
- **File Preview**: Shows the first rows of your data after loading
- **Automatic Detection**: Identifies numeric columns for analysis

### Configuration Panel

Configure all EFA-Boosting parameters:

- **Number of Factors**: Select the number of factors to extract
- **Item Prefix**: Specify the naming pattern of your items (e.g.,
  “item” for item1, item2, etc.)
- **Rotation Method**: Choose from oblimin, varimax, promax, etc.
- **Fit Targets**: Set target values for RMSEA, SRMR, and CFI
- **Advanced Options**:
  - Minimum loading threshold
  - Cross-loading threshold
  - Minimum items per factor
  - Global search options

### Exclude Items

You can specify items to exclude from the analysis before running. Enter
item names separated by commas (e.g., “item5, item12, item23”).

### Console Output

The real-time console displays:

- Algorithm progress at each iteration
- Items being evaluated and removed
- Fit index values
- Convergence information

The console features smart auto-scroll that only scrolls when you’re
near the bottom, allowing you to review earlier output while the
analysis runs.

### Results Tabs

#### Structure Tab

Displays the final factor loadings matrix in an interactive DataTable
format:

- Sortable columns
- Search functionality
- Loading values with highlighting

#### Removed Items Tab

Shows all items removed during optimization:

- Item name
- Removal reason (Heywood, cross-loading, low loading, etc.)
- Iteration number

#### Fit Indices Tab

Presents the final model fit indices:

- RMSEA with 90% CI
- SRMR
- CFI
- TLI
- Chi-square and df

### Export Options

- **Export Structure**: Download the final factor structure as CSV
- **Export Log**: Download the complete console output

## Workflow Example

1.  **Load Data**: Click “Browse” and select your data file (CSV or
    Excel)

2.  **Configure Analysis**:

    - Set the number of factors based on your theoretical model
    - Enter the item prefix that matches your column names
    - Adjust fit targets if needed (defaults are appropriate for most
      cases)

3.  **Optional - Exclude Items**: If you know certain items should not
    be included, list them in the exclude field

4.  **Run Analysis**: Click the “Run EFA-Boosting” button

5.  **Monitor Progress**: Watch the console for real-time updates

6.  **Review Results**:

    - Check the Structure tab for final loadings
    - Review removed items and reasons
    - Verify fit indices meet your criteria

7.  **Export**: Download the structure and/or log files for your records

## Required Packages

EFA-Boosting Studio requires these packages (automatically checked on
launch):

- shiny, bslib (UI framework)
- DT (interactive tables)
- readxl, readr (data import)
- ggplot2 (visualizations)
- future, promises (async execution)

## Tips

- **Large datasets**: The global search option can be computationally
  intensive. Consider starting with greedy search first.

- **Item naming**: Ensure your items follow a consistent naming pattern
  (e.g., item1, item2 or q1, q2)

- **Fit targets**: Start with default targets. Only adjust if you have
  specific requirements.

- **Console review**: The smart scroll allows you to scroll up and
  review earlier output without being forced back to the bottom.
