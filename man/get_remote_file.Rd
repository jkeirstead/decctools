\name{get_remote_file}
\alias{get_remote_file}
\title{Gets a file from a remote URL}
\usage{
get_remote_file(url, dir, update_cache = FALSE)
}
\arguments{
  \item{url}{the address of the file to download}

  \item{dir}{the directory to store the cached file in.  If
  undefined, it defaults to $TEMP/decctools.}

  \item{update_cache}{boolean.  Should the cached file be
  overwritten?  Default = FALSE}
}
\value{
the file name (path)
}
\description{
Since the URLs of UK government data appear to change quite
frequently, the method will automatically cache a local
copy.
}

