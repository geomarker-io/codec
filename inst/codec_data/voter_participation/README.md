# Voter Participation Rates

[![latest github release](https://img.shields.io/github/v/release/geomarker-io/codec?sort=date&filter=voter_participation-*&display_name=tag&label=%5B%E2%98%B0%5D&labelColor=%238CB4C3&color=%23396175)](https://github.com/geomarker-io/codec/releases?q=voter_participation&expanded=false)

Voter lists exported from https://votehamiltoncountyohio.gov are downloaded and converted into census-tract level voting participation rates for recent elections.

Versions are tagged based on the semantic version of the code and the date on which the data is based separated by a hypen (e.g., `0.1-20241104` is the 0.1 version of the code that downloaded the voter records on November 4th, 2024).

- Use https://votehamiltoncountyohio.gov/download.php?file=VoterListExport-20240905-no.csv pattern to download files; data will change depending on date of download, but script always uses the current date. (Note that historical voter records are not available using the VoterListExport URLs.)
- Assume that each voter's address represents their most recently registered address and may not reflect their registered address during older elections.
- Concatenate `AddressPreDirectional`, `AddressNumber`, `AddressStreet`, `AddressSuffix`, `CityName`, `"OH"` and `AddressZip` to create `voter_address`.
- Use the "[voting history legend](https://votehamiltoncountyohio.gov/campaign-media/voter-lists/)" described in the data documentation to dichotomize the voting status for each voter in each election. Voters were considered to have participated if they voted (either in person on election day, absentee, or early at an Early Vote Center) regardless of declared party affiliation (i.e., a non-missing value equals voter participation).
- Use the address matching and street range geocoding in the parcel pacakge to match each `voter_address` with a Hamilton County census tract.
- Calculate census tract rates of voter participation as the number of registered voters who participated divided by the total number of registered voters. 

More info: https://votehamiltoncountyohio.gov/campaign-media/voter-lists/