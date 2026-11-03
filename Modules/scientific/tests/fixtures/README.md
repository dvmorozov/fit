<!-- SPDX-License-Identifier: CC-BY-4.0 -->
# Real files, so the readers are held to what instruments write

Every file here was fetched **once, by hand**, from a public dataset, and is kept
whole: the point of reading one in a test is that the numbers come out as the
instrument wrote them. Git stores the XML with LF line endings where the
instrument wrote CRLF; nothing else differs, and XML does not care.

| File | Fetched from | Licence |
|---|---|---|
| `sand-718-44.xrdml` | `XRD-718-44_1.xrdml` of *Thermo-physical and thermo-optical properties of Arabian sands*, Zenodo, [doi:10.5281/zenodo.4892237](https://doi.org/10.5281/zenodo.4892237) | CC-BY-4.0 |

Recorded on **2026-10-20**, with:

    curl -sL -o sand-718-44.xrdml "https://zenodo.org/api/records/4892237/files/XRD-718-44_1.xrdml/content"

A PANalytical Empyrean scan, 2Theta 4.00656514 to 69.99936587 degrees, 5027
intensities in counts.
