# find-read-codes
Search UK Biobank clinical read 2/3 code lookup tables for codes beginning with a character or character vector input by the user. Return codes and descriptions.
Uploaded for personal use but should still be functional.


<h2>Requirements:</h2> <h3>Upload files read2_lkp.csv and read3_lkp.csv to working environment prior to running function.</h3>
<b>read2_lkp.csv:</b></p>
Headers are read_2, term_code, term_description, orig_read_code</p>
Some of the entries in term_description include a comma, so have been inadvertantly split between term_description and orig_read_code because this is a CSV. The function fixes this.</p>
<b>read3_lkp.csv:</b></p>
Headers are read_3, term_description, description_type, status, old_readcode</p>
Some of the entries in term_description include a comma, so have been inadvertantly split between term_description and description_type because this is a CSV. The function fixes this.</p>

<h2>Inputs:</h2> <h3>find_read_codes(codes, file1 = 'read2_lkp.csv', file2 = 'read3_lkp.csv', loop_limit = 3)</h3>
<b>codes</b> - A character or character vector containing the read2/3 codes to search for in the lookup tables. Codes can be no more than 5 character each. The function also finds read codes which start with codes input by the user. So codes = c('B13', 'B575', 'X902z') would find read 2 and 3 codes starting with B13 and B575, and matching X902z.</p>
<b>file1</b> - file path to read2_lkp.csv (or whatever you've named it).</p>
<b>file2</b> - file path to read3_lkp.csv (or whatever you've named it).</p>
<b>loop_limit</b> - a whole integer, defaults to 3 and recommended to be less than 5. Example usage: if code AAAAA has the term_description 'See BBBBB', this function will also search for read2/3 codes matching BBBBB. In the unlikely scenario that code AAAAA says 'See BBBBB', and code BBBBB says 'See CCCCC', and code CCCCC says 'See DDDDD'... etc., loop_limit limits the number of iterations. Also, in the unlikely scenario that code AAAAA says 'See BBBBB' and code BBBBB says 'See AAAAA', loop_limit prevents an endless while loop from occuring inside the function.</p>

<h2>Outputs:</h2> <h3>a dataframe with headers 'code', 'term_description_r2' and 'term_description_r3'</h3>
<b>code</b> - the read code</p>
<b>term_description_r2</b> - description of the read code according to read version 2.</p>
<b>term_description_r3</b> - description of the read code according to read version 3.</p>
Note that some read codes have multiple descriptions - these are combined into the same field and separated with a semicolon. If the description for one code is 'See [different code]', then the referenced code and its description will be added to the bottom of the dataframe.</p>

<h2>Example use case:</h2>
A researcher needs to extract UK Biobank GP records for any patients with colorectal cancer. A list of colorectal cancer codes is reported online: https://clinicalcodes.rss.mhs.man.ac.uk/medcodes/article/17/codelist/res17-colorectal-cancer/ </p>
<b>1.</b> From a list such as the one above, determine which codes/starting sequences of codes to search for, e.g. B13, B14, B575, B1z, B803, B804, B902, BB5N</p>
<b>2.</b> Make sure read2_lkp.csv and read3_lkp.csv are in the working environment</p>
<b>3.</b> Run in R:</p>
read_codes <- find_read_codes(c('B13','B14','B575','B1z','B803','B804','B902','BB5N'))</p>
  Running the above code will use the default/invisible settings of:</p>
  find_read_codes(c('B13','B14','B575','B1z','B803','B804','B902','BB5N'), file1 = 'read2_lkp.csv', file2 = 'read3_lkp.csv', loop_limit = 3)</p>
  File names and loop_limit can be specified if necessary.</p>
<b>4.</b> Output: a table of any read codes starting with the above listed codes, plus their descriptions.</p>
<b>5.</b> The researcher can manually look through output table to determine which codes are relevant.</p>
