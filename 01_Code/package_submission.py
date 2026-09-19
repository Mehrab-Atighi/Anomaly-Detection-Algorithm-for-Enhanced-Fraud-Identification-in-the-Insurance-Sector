"""Assemble a portable editable-source-only journal ZIP and source diff.

No PDF, historical exports or private professor report enter this ZIP.
Run after build_documents.py so the BBLs correspond to the current source.
"""
from pathlib import Path
import zipfile, re, difflib, json, hashlib
ROOT=Path(__file__).resolve().parents[1]
PAPER=ROOT/'05_Manuscript'
REPORTS=ROOT/'06_Reviewer_and_Professor'

def flatten(path,stack=()):
    path=path.resolve()
    if path in stack: raise ValueError('Recursive input: '+str(path))
    text=path.read_text()
    def expand(match):
        relative=match.group(1)
        target=PAPER/relative
        if not target.suffix:target=target.with_suffix('.tex')
        return '\n% BEGIN INCLUDED '+relative+'\n'+flatten(target,stack+(path,))+'\n% END INCLUDED '+relative+'\n'
    return re.sub(r'\\input\{([^}]+)\}',expand,text)

def main():
    flat=flatten(PAPER/'manuscript_clean.tex')
    (REPORTS/'revised_source_flattened.tex.txt').write_text(flat)
    original=(ROOT/'07_Original_Archive/Submitted_V5_Confirmed/v5.tex').read_text()
    diff=''.join(difflib.unified_diff(original.splitlines(True),flat.splitlines(True),
                 fromfile='Original_Submission/v5.tex',tofile='Revised/manuscript_clean_flattened.tex'))
    (REPORTS/'source_changes.diff').write_text(diff)
    section_names=re.findall(r'\\section\{([^}]+)\}',flat)
    expected=['Introduction','LITERATURE REVIEW','METHODOLOGY','Real Data Application','CONCLUSION']
    assert section_names==expected,(section_names,expected)
    assert flat.count(r'\begin{algorithm}')==3
    bib='\n'.join((PAPER/n).read_text() for n in ['refs.bib','references_additions.bib'])
    keys=set(re.findall(r'@\w+\s*\{\s*([^,]+),',bib))
    cited=set()
    for group in re.findall(r'\\cite\w*\*?(?:\[[^]]*\])*\{([^}]+)\}',flat):cited.update(k.strip() for k in group.split(','))
    assert len(keys)==40 and cited==keys,dict(keys=len(keys),missing=list(cited-keys),unused=list(keys-cited))
    files={}
    for path in PAPER.rglob('*'):
        if not path.is_file():continue
        rel=path.relative_to(PAPER)
        if path.suffix in {'.tex','.bib','.bbl','.sty'} or rel.parts[0]=='third_party_algorithms':
            files[str(rel)]=path.read_bytes()
    used_figures=set(re.findall(r'\\paperfigure\{([^}]+)\}',flat))
    direct=re.findall(r'\\includegraphics(?:\[[^\]]*\])?\{figures/([^}]+)\.png\}',flat)
    used_figures.update(n for n in direct if '#' not in n)
    assert len(used_figures)==10
    for name in used_figures: files['figures/'+name+'.png']=(PAPER/'figures'/f'{name}.png').read_bytes()
    for name in ['Simple.drawio','Multilayered.drawio']:
        files['figure_sources/'+name]=(ROOT/'04_Figures'/name).read_bytes()
    files['main.bbl']=(PAPER/'manuscript_clean.bbl').read_bytes()
    response=(REPORTS/'response_to_reviewer.tex').read_text().replace('../05_Manuscript/generated/numbers.tex','generated/numbers.tex')
    files['response_to_reviewer.tex']=response.encode()
    files['change_map.tex']=(REPORTS/'change_map.tex').read_bytes()
    files['README_SUBMISSION.txt']=('FINAL EDITABLE JOURNAL SOURCE\n\n'
        'Main clean manuscript: main.tex (loads manuscript_clean.tex).\n'
        'Marked manuscript: manuscript_marked.tex.\n'
        'Response: response_to_reviewer.tex.\n'
        'Readable change map: change_map.tex.\n'
        'Choose pdfLaTeX + BibTeX, or latexmk -pdf main.tex.\n'
        'Upload editable sources and PNG assets as required by Editorial Manager.\n'
        'There are deliberately no PDFs in this submission ZIP.\n'
        'The full research package separately contains Python modeling, executed R/ggplot2 plotting scripts, data, outputs and reading copies.\n'
        'Scope: revisions within the original article; five sections and Algorithms 1-3 retained.\n'
        'The optional marked source and change map can be assigned their corresponding file types.\n'
        'Third-party algorithms styles are bundled unchanged with their source and LGPL license.\n').encode()
    output=ROOT/'00_JOURNAL_SOURCE_ONLY.zip'
    with zipfile.ZipFile(output,'w',compression=zipfile.ZIP_DEFLATED,compresslevel=9) as z:
        for name,content in sorted(files.items()):z.writestr(name,content)
    with zipfile.ZipFile(output) as z:
        assert z.testzip() is None
        assert not any(n.lower().endswith('.pdf') for n in z.namelist())
    report={'status':'PASS','sections':section_names,'formal_algorithms':3,'cited_references':len(cited),
            'paper_figures':len(used_figures),'source_zip_files':len(files),'pdf_files_in_source_zip':0,
            'source_zip_sha256':hashlib.sha256(output.read_bytes()).hexdigest()}
    (ROOT/'08_Documentation/submission_structure_report.json').write_text(json.dumps(report,indent=2))
    print(json.dumps(report,indent=2))

if __name__=='__main__':main()
