import sys
from bs4 import BeautifulSoup, Comment
from urllib.parse import urljoin

def clean_html(html, base_url):
    soup = BeautifulSoup(html, 'html.parser')

    # Supprimer <script> et <style>
    for script_or_style in soup(['script', 'style']):
        script_or_style.decompose()

    # Supprimer les commentaires
    for comment in soup.find_all(string=lambda text: isinstance(text, Comment)):
        comment.extract()

    # Supprimer les attributs inutiles (garder href, src, alt, title)
    for tag in soup.find_all(True):
        attrs = dict(tag.attrs)
        for attr in attrs:
            if attr not in ['href', 'src', 'alt', 'title']:
                del tag.attrs[attr]

    for tag in soup.find_all(True):
        if 'href' in tag.attrs:
            tag['href'] = urljoin(base_url, tag['href'])
        if 'src' in tag.attrs:
            tag['src'] = urljoin(base_url, tag['src'])

    return str(soup)

def extract_body_content(html):
    soup = BeautifulSoup(html, 'html.parser')
    body_content = soup.find(id='bodyContent')
    if body_content:
        return str(body_content)
    else:
        return ''

html = sys.stdin.read()
base_url = sys.argv[1] if len(sys.argv) > 1 else ""

content = extract_body_content(html)
cleaned = clean_html(content, base_url)
print(cleaned)
