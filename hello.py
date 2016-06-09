from cherrypy import wsgiserver2

def app(environ, start_response):
    status = '200 OK'
    response_headers = []
    start_response(status, response_headers)
    return ['Hello world!']

server = wsgiserver2.CherryPyWSGIServer(
    ('0.0.0.0', 8334), app,
    server_name='www.cherrypy.example')
server.start()
