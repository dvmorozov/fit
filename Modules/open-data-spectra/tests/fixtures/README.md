<!-- SPDX-License-Identifier: CC-BY-4.0 -->
# Recorded answers, so no test needs the network

Every file here was fetched **once, by hand**, and cut down to what the source
actually reads. Nothing in this module's suite fetches anything: a test that
reached a public service would fail when that service is busy or rate-limits it,
and pass tomorrow for reasons nobody here decided.

Recorded on **2026-09-23**, with:

    curl -sS "https://webbook.nist.gov/cgi/cbook.cgi?Name=benzene&Units=SI"   # nist-species-benzene.html
    curl -sS "https://webbook.nist.gov/cgi/cbook.cgi?Name=phenol&Units=SI"    # nist-matches-phenol.html
    curl -sS "https://webbook.nist.gov/cgi/cbook.cgi?Name=quartz&Units=SI"    # nist-not-found.html
    curl -sS "https://webbook.nist.gov/cgi/cbook.cgi?JCAMP=C71432&Index=0&Type=IR"   # benzene-ir.jdx

`benzene-ir.jdx` is whole and unedited: it is a real spectrum, and the point of
reading it in a test is that the numbers come out as NIST wrote them. The HTML
files keep their `<title>` and the links the source looks for, and nothing else.

**When one of these stops matching the live service**, the source will start
refusing in words rather than guessing - that is what its limitation says - and
the fix is to re-record with the commands above and correct the reader.
