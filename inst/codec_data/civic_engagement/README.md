# Civic Engagement

Voter lists exported from https://votehamiltoncountyohio.gov are downloaded and converted into census-tract level voting participation rates for recent elections.

- Use https://votehamiltoncountyohio.gov/download.php?file=VoterListExport-20240905-no.csv pattern to download files; data will change depending on date of download.
- Assume that each voter's address represents their most recently registered address and may not reflect their registered address during older elections.
- Concatenate `AddressPreDirectional`, `AddressNumber`, `AddressStreet`, `AddressSuffix`, `CityName`, `"OH"` and `AddressZip` to create `voter_address`.
- Use the "[voting history legend](https://votehamiltoncountyohio.gov/campaign-media/voter-lists/)" described in the data documentation to dichotomize the voting status for each voter in each election. Voters were considered to have participated if they voted (either in person on election day, absentee, or early at an Early Vote Center) regardless of declared party affiliation (i.e., a non-missing value equals voter participation).
- Use the `DeGAUSS` geocoder v3.3.0 to match each `voter_address` with a Hamilton County census tract.
- Calculate census tract rates of voter participation as the number of registered voters who participated divided by the total number of registered voters. 

More info: https://votehamiltoncountyohio.gov/campaign-media/voter-lists/
