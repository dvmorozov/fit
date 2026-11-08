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

## `tape-backing-ftir.spc`

A Galactic SPC file as an FTIR spectrometer wrote it, whole and unedited:
`020_Backing_Pristine_3.SPC` of *Reliability of Forensic Tape Examination:
Post-Blast Effects - FTIR data - Pristine Part 2*, Zenodo,
[doi:10.5281/zenodo.20218668](https://doi.org/10.5281/zenodo.20218668),
licensed CC-BY-4.0. Recorded on **2026-10-20** with:

    curl -sL -o tape-backing-ftir.spc "https://zenodo.org/api/records/20218668/files/020_Backing_Pristine_3.SPC/content"

New-format (0x4B) header, one spectrum of 6950 points, wavenumbers 4000.1884765625
down to 649.9039916992188, absorbance as 32-bit integers scaled by 2^(0-32).
