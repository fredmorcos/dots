[ -d /home/fred/.cabal/bin ] && PATH=/home/fred/.cabal/bin:$PATH

[ -d /home/fred/Workspace/bin ] && PATH=/home/fred/Workspace/bin:$PATH

# [ -d /home/fred/.gem/ruby/2.1.0/bin ] && PATH=/home/fred/.gem/ruby/2.1.0/bin:$PATH
# [ -d /home/fred/node_modules/.bin ] && PATH=/home/fred/node_modules/.bin:$PATH

export PATH

# [ -d /home/fred/node_modules/jsdom/lib ] && NODE_PATH=/home/fred/node_modules/jsdom/lib:$NODE_PATH
# [ -d /home/fred/node_modules/xmldom ] && NODE_PATH=/home/fred/node_modules/xmldom:$NODE_PATH
# [ -d /home/fred/node_modules/ ] && NODE_PATH=/home/fred/node_modules/:$NODE_PATH

# export NODE_PATH

MOZ_USE_OMTC=1
export MOZ_USE_OMTC
