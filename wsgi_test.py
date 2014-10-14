# -*- coding: utf-8 -*-

from wsgiref.simple_server import make_server

portNum = 8000

html = """
<BODY>
テーブルのテストです。
<TABLE border="1">
<TR><TD>1</TD><TD>hai</TD></TR>
<TR><TD>row2 column1</TD><TD>row2 column2</TD></TR>
<TR><TD>行3</TD><TD>列3</TD></TR>
</TABLE>
</BODY>
"""

def custom_app(environ, start_response):
    status = '200 OK'
#    body = b"My Application"
    body = html.encode("utf-8")
    header = [('Content-Type', 'text/html;charset=utf-8')
              ,('Content-Length', str(len(body)))
              ]
    start_response(status, header)
    return [body]

httpd = make_server('', portNum, custom_app)
print(u"port", portNum, "でリクエスト受付中")
httpd.handle_request()
#httpd.serve_forever()
