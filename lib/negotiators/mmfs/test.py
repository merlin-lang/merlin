#!/usr/bin/python

import time
import socket
import subprocess

BUFSIZE = 512

cmd = ['mmfs.native', '-server',
       '-policy', 'mmfs/policy.mln', '-ipfile', 'mmfs/ips.txt']

# print "Running command: ", ' '.join(cmd)
# p = subprocess.Popen(cmd,stdout=subprocess.PIPE, stderr=subprocess.PIPE)
# out, err = p.communicate()
# print "Output:\n", out
# print "Error:\n", err


def single():
    ctrlr = socket.create_connection(("127.0.0.1", 1337))
    demander = socket.create_connection(("127.0.0.1", 7331))

    r = demander.recv(BUFSIZE)
    print "demander got: ", r

    r = ctrlr.recv(BUFSIZE)
    print "ctrlr got: ", r

    demands = open('mmfs/demands.txt').read()
    print "Demanding:\n%s\n" % demands
    demander.send(demands)

    r = ctrlr.recv(3*BUFSIZE)
    print "Ctrlr got: \n", r

    ctrlr.send("DONE")
    demander.send("DONE")

def rotate(ips, mins):
    mins = map(str, mins)

    # ctrlr = socket.create_connection(("127.0.0.1", 1337))
    demander = socket.create_connection(("127.0.0.1", 7331))

    r = demander.recv(BUFSIZE)
    print "demander got: ", r

    # r = ctrlr.recv(BUFSIZE)
    # print "ctrlr got: ", r

    for i in range(len(ips)):
        offset = i % len(mins)
        demands = []
        for j in range(len(mins)):
            line = ' '.join([ips[j], mins[(j + i) % len(mins)], "0", ";"])
            demands.append(line)

        demands = '\n'.join(demands)
        print "Demanding:\n%s\n" % demands

        demander.send(demands)
        # r = ctrlr.recv(3*BUFSIZE)
        # print "Ctrlr got policy: \n%s\n" % r
        time.sleep(2)


    # ctrlr.send("DONE")
    demander.send("DONE")

if __name__ == '__main__':

    ips = ["10.0.0.1", "10.0.0.2", "10.0.0.3"]
    mins = ["6000000", "200000", "200000"]
    rotate(ips, mins)
