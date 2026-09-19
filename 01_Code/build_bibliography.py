"""Build the additional bibliography from the bundled verified DOI metadata.

This offline step performs no searches and adds no unverified references.
The original 16 curated entries remain in refs.bib.
"""
from pathlib import Path
import json, re, html
ROOT=Path(__file__).resolve().parents[1]

def clean(s):
    s=html.unescape(re.sub('<[^>]+>', '', str(s)))
    return ' '.join(s.split()).replace('&',r'\&')

def main():
    metadata=json.loads((ROOT/'08_Documentation/added_reference_metadata.json').read_text())
    entries=[]
    for key,m in metadata.items():
        if 'error' in m: raise ValueError((key,m['error']))
        kind={'book':'book','proceedings-article':'inproceedings'}.get(m['type'],'article')
        title=clean(m['title'][0])
        if m.get('subtitle'): title+=': '+clean(m['subtitle'][0])
        authors=m['author']
        if key=='He2009': authors=[{'family':'He','given':'Haibo'},{'family':'Garcia','given':'Edwardo A.'}]
        fields={'author':' and '.join(clean(a['family'])+', '+clean(a.get('given','')) for a in authors),
                'title':'{'+title+'}', 'year':str(m['published']['date-parts'][0][0]), 'doi':m['DOI']}
        if kind=='book': fields.update(publisher='Springer',edition='2')
        else:
            fields['booktitle' if kind=='inproceedings' else 'journal']=clean(m['container-title'][0])
            for src,dst in [('volume','volume'),('issue','number'),('page','pages')]:
                if m.get(src): fields[dst]=clean(m[src]).replace('-', '--') if src=='page' else clean(m[src])
        if key=='Kuhn2008': fields['pages']='1--26'
        if key=='Bolton2002': fields['pages']='235--255'
        if key=='Chandola2009': fields['note']='Article 15'
        entries.append('@'+kind+'{'+key+',\n'+',\n'.join('  '+k+'={'+v+'}' for k,v in fields.items())+'\n}\n')
    entries.append(r'''@article{Pedregosa2011,
  author={Pedregosa, Fabian and Varoquaux, Ga{\"e}l and Gramfort, Alexandre and Michel, Vincent and Thirion, Bertrand and Grisel, Olivier and Blondel, Mathieu and Prettenhofer, Peter and Weiss, Ron and Dubourg, Vincent and Vanderplas, Jake and Passos, Alexandre and Cournapeau, David and Brucher, Matthieu and Perrot, Matthieu and Duchesnay, {\'E}douard},
  title={{Scikit-learn: Machine Learning in Python}},
  journal={Journal of Machine Learning Research},
  volume={12}, number={85}, pages={2825--2830}, year={2011},
  url={https://jmlr.org/papers/v12/pedregosa11a.html}
}
''')
    (ROOT/'05_Manuscript/references_additions.bib').write_text('\n'.join(entries))
    print('Built 24 additional references from verified metadata.')

if __name__=='__main__': main()
