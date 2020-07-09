#import sys
#import argparse
import os
from retic import String, Void, List
from paramiko_config import SSHConfig


NO_PORT = "-1" #bg

def parse(lines:List(String))->SSHConfig:
    parser = SSHConfig()
    parser.parse(lines)
    return parser

def get_netloc(entry:Tuple(List(String),Dict(String,String)), parser:SSHConfig)->Tuple(String,String):
    hostname = "".join(entry[0]) #bg
    if hostname == "*":
        return ("*", NO_PORT)
    port = parser.lookup(hostname).get('port')
    return (hostname, port)


def get_netlocs(lines:List(String))->Dict(String,String):
    parser = parse(lines)
    entries = parser._config
    netlocs = {}
    for entry in entries:
        netloc = get_netloc(entry, parser)
        if not netloc:
            continue
        hostname, port = netloc
        if port != NO_PORT:
            netlocs[hostname] = port
    return netlocs


def execute(lines:List(String), template_file:String)->String:
    netlocs = get_netlocs(lines)

    #bg simplified
    dirpath, filename = os.path.split(template_file)
    template_context = [
        (hostname, '%s:%s' % (hostname, port))
        for hostname, port in netlocs.items()
    ]
    return str(sorted(template_context, key=lambda x: x[0]))
