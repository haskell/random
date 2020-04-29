#! /usr/bin/env nix-shell
#! nix-shell -i python3 -p python3 python37Packages.pandas

# Usage: compare.py <reference.csv> <results.csv>
#
# Compares the benchmark results in the second argument with those in the first
# argument. Outputs a comparison of those rows where the results differ
# significantly.

import pandas as pd

# Threshold for the relative difference, as a percentage
SIGNIFICANT = 20

if __name__ == "__main__":
    from sys import argv

    ref = pd.read_csv(argv[1])
    res = pd.read_csv(argv[2])

    # v1.1 benchmarks call it 'randomR', newer ones 'uniformR'. They are
    # directly comparable. We normalise to 'uniformR' here.
    ref["Name"] = ref["Name"].str.replace("randomR", "uniformR")
    res["Name"] = res["Name"].str.replace("randomR", "uniformR")
    comp = pd.merge(
        ref,
        res,
        on="Name",
        how="outer",
        validate="one_to_one",
        suffixes=("_ref", "_res"),
    )

    diff_col = "Diff_rel"
    comp[diff_col] = (comp["Mean_ref"] - comp["Mean_res"]) / comp["Mean_res"]

    cols = ["Name", "Mean_ref", "Mean_res", diff_col]
    print_opts = {
        "index": False,
        "columns": cols,
        "formatters": {diff_col: "{:,.0%}".format},
    }
    significant_percent = SIGNIFICANT / 100

    slower = comp[comp[diff_col] < -significant_percent]
    if slower.empty:
        print("SLOWER: none")
    else:
        print("SLOWER")
        print(slower.to_string(**print_opts))

    print()

    faster = comp[comp[diff_col] > significant_percent]
    if faster.empty:
        print("FASTER: none")
    else:
        print("\nFASTER")
        print(faster.to_string(**print_opts))
