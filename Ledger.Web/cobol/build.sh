#!/usr/bin/env bash
set -euo pipefail
mkdir -p cobol/bin
cobc -x -free -Wall -o cobol/bin/ledger_report cobol/ledger_report.cob
echo "Built cobol/bin/ledger_report"
