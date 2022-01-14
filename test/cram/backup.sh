#!/usr/bin/env sh

"$TESTDIR/vbu" --config config backup -v 2>&1 | sed -E -e 's/[0-9]+\.[0-9]+/$SECONDS/' -E -e 's/(Finished backing up [0-9]+ files? for files in \$SECONDSs on).*/\1 $DATE_AND_TIME/' -E -e 's/bak\.[0-9_]+/bak.$TIMESTAMP/' -E -e 's/[^ ]+cramtests-[^/]+/$TMP_DIR/' | sort