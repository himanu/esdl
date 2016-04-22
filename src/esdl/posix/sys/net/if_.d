/* Device mapping structure. I'd just gone off and designed a
   beautiful scheme using only loadable modules with arguments for
   driver options and along come the PCMCIA people 8)

   Ah well. The get() side of this is good for WDSETUP, and it'll be
   handy for debugging things. The set side is fine for now and being
   very small might be worth keeping for clean configuration.  */

module esdl.posix.sys.net.if_;

import core.sys.posix.sys.socket: sockaddr;

struct ifmap
{
  ulong mem_start;
  ulong mem_end;
  ushort base_addr;
  ubyte irq;
  ubyte dma;
  ubyte port;
  /* 3 bytes spare */
};

/* Interface request structure used for socket ioctl's.  All interface
   ioctl's must have parameter definitions which begin with ifr_name.
   The remainder may be interface specific.  */

enum IF_NAMESIZE = 16;
enum IFHWADDRLEN = 6;
enum IFNAMSIZ    = IF_NAMESIZE;

/* TUNSETIFF ifr flags */
enum IFF_TUN		= 0x0001;
enum IFF_TAP		= 0x0002;
enum IFF_NO_PI	        = 0x1000;
/* This flag has no real effect */
enum IFF_ONE_QUEUE	= 0x2000;
enum IFF_VNET_HDR	= 0x4000;
enum IFF_TUN_EXCL	= 0x8000;
enum IFF_MULTI_QUEUE    = 0x0100;
enum IFF_ATTACH_QUEUE   = 0x0200;
enum IFF_DETACH_QUEUE   = 0x0400;
/* read-only flag */
enum IFF_PERSIST	= 0x0800;
enum IFF_NOFILTER	= 0x1000;

/* receive all packets		*/
enum IFF_PROMISC	= 0x100;

struct ifreq
{
  union
  {
    char[IFNAMSIZ] ifrn_name; /* Interface name, e.g. "en0".  */
  };

  union
  {
    sockaddr ifru_addr;
    sockaddr ifru_dstaddr;
    sockaddr ifru_broadaddr;
    sockaddr ifru_netmask;
    sockaddr ifru_hwaddr;
    short ifru_flags;
    int ifru_ivalue;
    int ifru_mtu;
    ifmap ifru_map;
    char[IFNAMSIZ] ifru_slave;	/* Just fits the size */
    char[IFNAMSIZ] ifru_newname;
    char* ifru_data;
  };
};

