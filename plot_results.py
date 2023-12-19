from argparse import ArgumentParser, Namespace
from pathlib import Path

import matplotlib.pyplot as plt
import pandas as pd

def parse_args() -> Namespace:
    parser = ArgumentParser(
        "FortranSimpleModelPlot",
        description="Generates a plot of the SEIRS model over time",
    )

    parser.add_argument(
        "input",
        type=Path,
        help="The input .csv to plot",
    )

    parser.add_argument(
        "-o",
        "--output",
        type=Path,
        default="SEIRS.png",
        help="Destination of the saved plot",
    )

    return parser.parse_args()


def plot(df: pd.DataFrame, output: Path) -> None:
    for col in "SEIR":
        plt.plot(df["time"], df[col], label=col)
    plt.legend()
    plt.xlabel("Time /years")
    plt.ylabel("Population Fraction")
    plt.savefig(output)


if __name__ == "__main__":
    args = parse_args()
    df = pd.read_csv(args.input)
    plot(df, args.output)
