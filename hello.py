from cherrypy import wsgiserver2

def app(environ, start_response):
    start_response('200 OK', [])
    return ['Hello!']

server = wsgiserver2.CherryPyWSGIServer(
    ('0.0.0.0', 8334), app,
    server_name='www.cherrypy.example')

server.start()
