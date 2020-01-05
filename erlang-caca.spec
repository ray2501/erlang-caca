#
# spec file for package erlang-caca
#

%define app_name caca
Name:           erlang-%{app_name}
Version:        0.2.0
Release:        0
%define app_ver %(echo "%{version}" | cut -d "+" -f1)
Summary:        Erlang bindings for libcaca
License:        MIT
Group:          Development/Libraries/Other
Url:            https://github.com/ray2501/erlang-caca
Source:         %{name}-%{version}.tar.gz
BuildRequires:  make
BuildRequires:  gcc
BuildRequires:  pkgconfig
BuildRequires:  pkgconfig(caca)
BuildRequires:  erlang
Requires:       erlang
BuildRoot:      %{_tmppath}/%{name}-%{version}-build

%description
It is an Erlang bindings for libcaca.

%prep
%setup -q -n %{name}-%{version}

%build
make

%install
for dir in ebin include priv ; do
	mkdir -p %{buildroot}%{erlang_libdir}/%{app_name}-%{app_ver}/${dir}
	cp -r ${dir}/* %{buildroot}%{erlang_libdir}/%{app_name}-%{app_ver}/${dir}/
done

%files
%defattr(-,root,root)
%doc README.md LICENSE
%dir %{erlang_libdir}/%{app_name}-%{app_ver}
%dir %{erlang_libdir}/%{app_name}-%{app_ver}/ebin
%{erlang_libdir}/%{app_name}-%{app_ver}/ebin/*.app
%{erlang_libdir}/%{app_name}-%{app_ver}/ebin/*.beam
%dir %{erlang_libdir}/%{app_name}-%{app_ver}/include
%{erlang_libdir}/%{app_name}-%{app_ver}/include/*.hrl
%dir %{erlang_libdir}/%{app_name}-%{app_ver}/priv
%{erlang_libdir}/%{app_name}-%{app_ver}/priv/*.so

%changelog

